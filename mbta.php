<?php
//MBTA data mining script

//configuration
//db config
$dbhost = '';
$dbuser = '';
$dbdb   = 'mbta';
$dbpass = '';

//api config
$baseurl = 'http://realtime.mbta.com/developer/api/v2/';
$key     = '';

//grab command line options
$options = getopt('qd');

//check to see if we should skip databasing
$database = !array_key_exists('d', $options);


//check to see if we should skip printing results
global $quiet;
$quiet = !array_key_exists('q', $options);


if($database) {
	//connect to the mysql database and return the handle
	$database = @mysqli_connect($dbhost, $dbuser, $dbpass, $dbdb);

	//give up if it failed
	if (mysqli_connect_errno()) {
		die('Database failed to connect.');
	}
} else {
	say('Skipping database.'.PHP_EOL);
}
/*
//Get the route IDs
//no point in running this check repeatedly, here just for reference
$request = "{$baseurl}routes?api_key={$key}&format=json";
$routelist = json_decode(file_get_contents($request),TRUE);
var_dump($routelist['mode'][0]); //green line routes
var_dump($routelist['mode'][1]); //red line routes
*/

//List and name all the routes you're tracking
$routes = array(array('Red Line, Braintree'  => '933_'),
                array('Red Line, Ashmont'    => '931_'),
                array('Green Line'           => '810_'),
                array('Green Line, B Branch' => '813_'),
                array('Green Line'           => '823_'),
                array('Green Line, C Branch' => '830_'),
                array('Green Line, C Branch' => '831_'),
                array('Green Line, Park?'    => '840_'),
                array('Green Line'           => '842_'),
                array('Green Line, D Branch' => '851_'),
                array('Green Line, D Branch' => '852_'),
                array('Green Line, E Branch' => '880_'),
                array('Green Line'           => '882_'),
                array('Green Line'           => '899_'));

//iterate through each given route
foreach($routes as $route) {
    foreach($route as $tag => $id) {
		say("Querying {$tag} ({$id})...");
		//prepare the api call
		$request = "{$baseurl}vehiclesbyroute?api_key={$key}&route={$id}&format=json";
		//make the call and confirm that it worked
		if(!@$predictions = json_decode(file_get_contents($request),TRUE)) {
			say("No trains on this line at the moment.");
			break; //break the loop early, check the next route
		}
		//if the data came back, then check both directions
		foreach($predictions['direction'] as $direction) {
			//then check each train in each direction
			foreach($direction['trip'] as $trip) {
				//extract the train from the trip
				$train = $trip['vehicle'];
				say($trip['trip_headsign']);
				say('Vehicle ID: '.$train['vehicle_id']);
				if($database) {
					//make a unique hash for the database
					$uid = hash('md5', microtime(TRUE));
					//prepare the query to insert the data
					$query = "INSERT INTO mbta.vehicle (uid,vehicle_id,vehicle_lat,vehicle_lon,vehicle_bearing,vehicle_timestamp,trip_id,trip_headsign,direction_id_fk,route_id_fk) VALUES('{$uid}','{$train['vehicle_id']}','{$train['vehicle_lat']}','{$train['vehicle_lon']}','{$train['vehicle_bearing']}','{$train['vehicle_timestamp']}','{$trip['trip_id']}','{$trip['trip_headsign']}','{$direction['direction_id']}','{$id}')";
					//attempt to insert the data
					if(!mysqli_query($database, $query)) {
						//kill the script if this fails, since that implies something else is wrong
						die('Failed to query database!'.PHP_EOL."Query: {$query}".PHP_EOL);
					}
				}
			}
		}
    }
}

//function for setting when to print output to console
function say($say) {
	global $quiet;
	if (!$quiet) {
		echo $say.PHP_EOL;
	}
}
?>

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.CreateFleet
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new fleet to host game servers. A fleet consists of a set of
-- Amazon Elastic Compute Cloud (Amazon EC2) instances of a certain type,
-- which defines the CPU, memory, storage, and networking capacity of each
-- host in the fleet. See
-- <https://aws.amazon.com/ec2/instance-types/ Amazon EC2 Instance Types>
-- for more information. Each instance in the fleet hosts a game server
-- created from the specified game build. Once a fleet is in an ACTIVE
-- state, it can begin hosting game sessions.
--
-- To create a new fleet, provide a name and the EC2 instance type for the
-- new fleet, and specify the build and server launch path. Builds must be
-- in a READY state before they can be used to build fleets. When
-- configuring the new fleet, you can optionally (1) provide a set of
-- launch parameters to be passed to a game server when activated; (2)
-- limit incoming traffic to a specified range of IP addresses and port
-- numbers; (3) set game session protection for all instances in the fleet,
-- and (4) configure Amazon GameLift to store game session logs by
-- specifying the path to the logs stored in your game server files. If the
-- call is successful, Amazon GameLift performs the following tasks:
--
-- -   Creates a fleet record and sets the state to NEW.
-- -   Sets the fleet\'s capacity to 1 \"desired\" and 1 \"active\" EC2
--     instance count.
-- -   Creates an EC2 instance and begins the process of initializing the
--     fleet and activating a game server on the instance.
-- -   Begins writing events to the fleet event log, which can be accessed
--     in the GameLift console.
--
-- Once a fleet is created, use the following actions to change certain
-- fleet properties (server launch parameters and log paths cannot be
-- changed):
--
-- -   < UpdateFleetAttributes> -- Update fleet metadata, including name
--     and description.
-- -   < UpdateFleetCapacity> -- Increase or decrease the number of
--     instances you want the fleet to maintain.
-- -   < UpdateFleetPortSettings> -- Change the IP addresses and ports that
--     allow access to incoming traffic.
module Network.AWS.GameLift.CreateFleet
    (
    -- * Creating a Request
      createFleet
    , CreateFleet
    -- * Request Lenses
    , cfServerLaunchParameters
    , cfLogPaths
    , cfEC2InboundPermissions
    , cfNewGameSessionProtectionPolicy
    , cfDescription
    , cfName
    , cfBuildId
    , cfServerLaunchPath
    , cfEC2InstanceType

    -- * Destructuring the Response
    , createFleetResponse
    , CreateFleetResponse
    -- * Response Lenses
    , cfrsFleetAttributes
    , cfrsResponseStatus
    ) where

import           Network.AWS.GameLift.Types
import           Network.AWS.GameLift.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input for a request action.
--
-- /See:/ 'createFleet' smart constructor.
data CreateFleet = CreateFleet'
    { _cfServerLaunchParameters         :: !(Maybe Text)
    , _cfLogPaths                       :: !(Maybe [Text])
    , _cfEC2InboundPermissions          :: !(Maybe [IPPermission])
    , _cfNewGameSessionProtectionPolicy :: !(Maybe ProtectionPolicy)
    , _cfDescription                    :: !(Maybe Text)
    , _cfName                           :: !Text
    , _cfBuildId                        :: !Text
    , _cfServerLaunchPath               :: !Text
    , _cfEC2InstanceType                :: !EC2InstanceType
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateFleet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cfServerLaunchParameters'
--
-- * 'cfLogPaths'
--
-- * 'cfEC2InboundPermissions'
--
-- * 'cfNewGameSessionProtectionPolicy'
--
-- * 'cfDescription'
--
-- * 'cfName'
--
-- * 'cfBuildId'
--
-- * 'cfServerLaunchPath'
--
-- * 'cfEC2InstanceType'
createFleet
    :: Text -- ^ 'cfName'
    -> Text -- ^ 'cfBuildId'
    -> Text -- ^ 'cfServerLaunchPath'
    -> EC2InstanceType -- ^ 'cfEC2InstanceType'
    -> CreateFleet
createFleet pName_ pBuildId_ pServerLaunchPath_ pEC2InstanceType_ =
    CreateFleet'
    { _cfServerLaunchParameters = Nothing
    , _cfLogPaths = Nothing
    , _cfEC2InboundPermissions = Nothing
    , _cfNewGameSessionProtectionPolicy = Nothing
    , _cfDescription = Nothing
    , _cfName = pName_
    , _cfBuildId = pBuildId_
    , _cfServerLaunchPath = pServerLaunchPath_
    , _cfEC2InstanceType = pEC2InstanceType_
    }

-- | Parameters required to launch your game server. These parameters should
-- be expressed as a string of command-line parameters. Example: \"+sv_port
-- 33435 +start_lobby\".
cfServerLaunchParameters :: Lens' CreateFleet (Maybe Text)
cfServerLaunchParameters = lens _cfServerLaunchParameters (\ s a -> s{_cfServerLaunchParameters = a});

-- | Path to game-session log files generated by your game server. Once a
-- game session has been terminated, Amazon GameLift captures and stores
-- the logs on Amazon S3. Use the GameLift console to access the stored
-- logs.
cfLogPaths :: Lens' CreateFleet [Text]
cfLogPaths = lens _cfLogPaths (\ s a -> s{_cfLogPaths = a}) . _Default . _Coerce;

-- | Access limits for incoming traffic. Setting these values limits game
-- server access to incoming traffic using specified IP ranges and port
-- numbers. Some ports in a range may be restricted. You can provide one or
-- more sets of permissions for the fleet.
cfEC2InboundPermissions :: Lens' CreateFleet [IPPermission]
cfEC2InboundPermissions = lens _cfEC2InboundPermissions (\ s a -> s{_cfEC2InboundPermissions = a}) . _Default . _Coerce;

-- | Game session protection policy to apply to all instances created in this
-- fleet. If this parameter is not set, new instances in this fleet will
-- default to no protection. Protection can be set for individual instances
-- using < UpdateGameSession>.
--
-- -   NoProtection: The game session can be terminated during a scale-down
--     event.
-- -   FullProtection: If the game session is in an ACTIVE status, it
--     cannot be terminated during a scale-down event.
cfNewGameSessionProtectionPolicy :: Lens' CreateFleet (Maybe ProtectionPolicy)
cfNewGameSessionProtectionPolicy = lens _cfNewGameSessionProtectionPolicy (\ s a -> s{_cfNewGameSessionProtectionPolicy = a});

-- | Human-readable description of the fleet.
cfDescription :: Lens' CreateFleet (Maybe Text)
cfDescription = lens _cfDescription (\ s a -> s{_cfDescription = a});

-- | Descriptive label associated with this fleet. Fleet names do not need to
-- be unique.
cfName :: Lens' CreateFleet Text
cfName = lens _cfName (\ s a -> s{_cfName = a});

-- | Unique identifier for the build you want the new fleet to use.
cfBuildId :: Lens' CreateFleet Text
cfBuildId = lens _cfBuildId (\ s a -> s{_cfBuildId = a});

-- | Path to the launch executable for the game server. A game server is
-- built into a 'C:\\game' drive. This value must be expressed as
-- 'C:\\game\\[launchpath]'. Example: If, when built, your game server
-- files are in a folder called \"MyGame\", your log path should be
-- 'C:\\game\\MyGame\\server.exe'.
cfServerLaunchPath :: Lens' CreateFleet Text
cfServerLaunchPath = lens _cfServerLaunchPath (\ s a -> s{_cfServerLaunchPath = a});

-- | Type of EC2 instances used in the fleet. EC2 instance types define the
-- CPU, memory, storage, and networking capacity of the fleetaposs hosts.
-- Amazon GameLift supports the EC2 instance types listed below. See
-- <https://aws.amazon.com/ec2/instance-types/ Amazon EC2 Instance Types>
-- for detailed descriptions of each.
cfEC2InstanceType :: Lens' CreateFleet EC2InstanceType
cfEC2InstanceType = lens _cfEC2InstanceType (\ s a -> s{_cfEC2InstanceType = a});

instance AWSRequest CreateFleet where
        type Rs CreateFleet = CreateFleetResponse
        request = postJSON gameLift
        response
          = receiveJSON
              (\ s h x ->
                 CreateFleetResponse' <$>
                   (x .?> "FleetAttributes") <*> (pure (fromEnum s)))

instance Hashable CreateFleet

instance NFData CreateFleet

instance ToHeaders CreateFleet where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("GameLift.CreateFleet" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateFleet where
        toJSON CreateFleet'{..}
          = object
              (catMaybes
                 [("ServerLaunchParameters" .=) <$>
                    _cfServerLaunchParameters,
                  ("LogPaths" .=) <$> _cfLogPaths,
                  ("EC2InboundPermissions" .=) <$>
                    _cfEC2InboundPermissions,
                  ("NewGameSessionProtectionPolicy" .=) <$>
                    _cfNewGameSessionProtectionPolicy,
                  ("Description" .=) <$> _cfDescription,
                  Just ("Name" .= _cfName),
                  Just ("BuildId" .= _cfBuildId),
                  Just ("ServerLaunchPath" .= _cfServerLaunchPath),
                  Just ("EC2InstanceType" .= _cfEC2InstanceType)])

instance ToPath CreateFleet where
        toPath = const "/"

instance ToQuery CreateFleet where
        toQuery = const mempty

-- | Represents the returned data in response to a request action.
--
-- /See:/ 'createFleetResponse' smart constructor.
data CreateFleetResponse = CreateFleetResponse'
    { _cfrsFleetAttributes :: !(Maybe FleetAttributes)
    , _cfrsResponseStatus  :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateFleetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cfrsFleetAttributes'
--
-- * 'cfrsResponseStatus'
createFleetResponse
    :: Int -- ^ 'cfrsResponseStatus'
    -> CreateFleetResponse
createFleetResponse pResponseStatus_ =
    CreateFleetResponse'
    { _cfrsFleetAttributes = Nothing
    , _cfrsResponseStatus = pResponseStatus_
    }

-- | Properties for the newly created fleet.
cfrsFleetAttributes :: Lens' CreateFleetResponse (Maybe FleetAttributes)
cfrsFleetAttributes = lens _cfrsFleetAttributes (\ s a -> s{_cfrsFleetAttributes = a});

-- | The response status code.
cfrsResponseStatus :: Lens' CreateFleetResponse Int
cfrsResponseStatus = lens _cfrsResponseStatus (\ s a -> s{_cfrsResponseStatus = a});

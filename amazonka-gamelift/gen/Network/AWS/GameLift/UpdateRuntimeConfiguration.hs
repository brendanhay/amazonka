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
-- Module      : Network.AWS.GameLift.UpdateRuntimeConfiguration
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the current run-time configuration for the specified fleet, which tells Amazon GameLift how to launch server processes on instances in the fleet. You can update a fleet's run-time configuration at any time after the fleet is created; it does not need to be in an @ACTIVE@ status.
--
--
-- To update run-time configuration, specify the fleet ID and provide a @RuntimeConfiguration@ object with the updated collection of server process configurations.
--
-- Each instance in a Amazon GameLift fleet checks regularly for an updated run-time configuration and changes how it launches server processes to comply with the latest version. Existing server processes are not affected by the update; they continue to run until they end, while Amazon GameLift simply adds new server processes to fit the current run-time configuration. As a result, the run-time configuration changes are applied gradually as existing processes shut down and new processes are launched in Amazon GameLift's normal process recycling activity.
--
-- Fleet-related operations include:
--
--     * 'CreateFleet'
--
--     * 'ListFleets'
--
--     * 'DeleteFleet'
--
--     * Describe fleets:
--
--     * 'DescribeFleetAttributes'
--
--     * 'DescribeFleetCapacity'
--
--     * 'DescribeFleetPortSettings'
--
--     * 'DescribeFleetUtilization'
--
--     * 'DescribeRuntimeConfiguration'
--
--     * 'DescribeEC2InstanceLimits'
--
--     * 'DescribeFleetEvents'
--
--
--
--     * Update fleets:
--
--     * 'UpdateFleetAttributes'
--
--     * 'UpdateFleetCapacity'
--
--     * 'UpdateFleetPortSettings'
--
--     * 'UpdateRuntimeConfiguration'
--
--
--
--     * Manage fleet actions:
--
--     * 'StartFleetActions'
--
--     * 'StopFleetActions'
--
--
--
--
--
module Network.AWS.GameLift.UpdateRuntimeConfiguration
    (
    -- * Creating a Request
      updateRuntimeConfiguration
    , UpdateRuntimeConfiguration
    -- * Request Lenses
    , urcFleetId
    , urcRuntimeConfiguration

    -- * Destructuring the Response
    , updateRuntimeConfigurationResponse
    , UpdateRuntimeConfigurationResponse
    -- * Response Lenses
    , urcrsRuntimeConfiguration
    , urcrsResponseStatus
    ) where

import Network.AWS.GameLift.Types
import Network.AWS.GameLift.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input for a request action.
--
--
--
-- /See:/ 'updateRuntimeConfiguration' smart constructor.
data UpdateRuntimeConfiguration = UpdateRuntimeConfiguration'
  { _urcFleetId              :: !Text
  , _urcRuntimeConfiguration :: !RuntimeConfiguration
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateRuntimeConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'urcFleetId' - Unique identifier for a fleet to update run-time configuration for.
--
-- * 'urcRuntimeConfiguration' - Instructions for launching server processes on each instance in the fleet. The run-time configuration for a fleet has a collection of server process configurations, one for each type of server process to run on an instance. A server process configuration specifies the location of the server executable, launch parameters, and the number of concurrent processes with that configuration to maintain on each instance.
updateRuntimeConfiguration
    :: Text -- ^ 'urcFleetId'
    -> RuntimeConfiguration -- ^ 'urcRuntimeConfiguration'
    -> UpdateRuntimeConfiguration
updateRuntimeConfiguration pFleetId_ pRuntimeConfiguration_ =
  UpdateRuntimeConfiguration'
    {_urcFleetId = pFleetId_, _urcRuntimeConfiguration = pRuntimeConfiguration_}


-- | Unique identifier for a fleet to update run-time configuration for.
urcFleetId :: Lens' UpdateRuntimeConfiguration Text
urcFleetId = lens _urcFleetId (\ s a -> s{_urcFleetId = a})

-- | Instructions for launching server processes on each instance in the fleet. The run-time configuration for a fleet has a collection of server process configurations, one for each type of server process to run on an instance. A server process configuration specifies the location of the server executable, launch parameters, and the number of concurrent processes with that configuration to maintain on each instance.
urcRuntimeConfiguration :: Lens' UpdateRuntimeConfiguration RuntimeConfiguration
urcRuntimeConfiguration = lens _urcRuntimeConfiguration (\ s a -> s{_urcRuntimeConfiguration = a})

instance AWSRequest UpdateRuntimeConfiguration where
        type Rs UpdateRuntimeConfiguration =
             UpdateRuntimeConfigurationResponse
        request = postJSON gameLift
        response
          = receiveJSON
              (\ s h x ->
                 UpdateRuntimeConfigurationResponse' <$>
                   (x .?> "RuntimeConfiguration") <*>
                     (pure (fromEnum s)))

instance Hashable UpdateRuntimeConfiguration where

instance NFData UpdateRuntimeConfiguration where

instance ToHeaders UpdateRuntimeConfiguration where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("GameLift.UpdateRuntimeConfiguration" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateRuntimeConfiguration where
        toJSON UpdateRuntimeConfiguration'{..}
          = object
              (catMaybes
                 [Just ("FleetId" .= _urcFleetId),
                  Just
                    ("RuntimeConfiguration" .=
                       _urcRuntimeConfiguration)])

instance ToPath UpdateRuntimeConfiguration where
        toPath = const "/"

instance ToQuery UpdateRuntimeConfiguration where
        toQuery = const mempty

-- | Represents the returned data in response to a request action.
--
--
--
-- /See:/ 'updateRuntimeConfigurationResponse' smart constructor.
data UpdateRuntimeConfigurationResponse = UpdateRuntimeConfigurationResponse'
  { _urcrsRuntimeConfiguration :: !(Maybe RuntimeConfiguration)
  , _urcrsResponseStatus       :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateRuntimeConfigurationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'urcrsRuntimeConfiguration' - The run-time configuration currently in force. If the update was successful, this object matches the one in the request.
--
-- * 'urcrsResponseStatus' - -- | The response status code.
updateRuntimeConfigurationResponse
    :: Int -- ^ 'urcrsResponseStatus'
    -> UpdateRuntimeConfigurationResponse
updateRuntimeConfigurationResponse pResponseStatus_ =
  UpdateRuntimeConfigurationResponse'
    { _urcrsRuntimeConfiguration = Nothing
    , _urcrsResponseStatus = pResponseStatus_
    }


-- | The run-time configuration currently in force. If the update was successful, this object matches the one in the request.
urcrsRuntimeConfiguration :: Lens' UpdateRuntimeConfigurationResponse (Maybe RuntimeConfiguration)
urcrsRuntimeConfiguration = lens _urcrsRuntimeConfiguration (\ s a -> s{_urcrsRuntimeConfiguration = a})

-- | -- | The response status code.
urcrsResponseStatus :: Lens' UpdateRuntimeConfigurationResponse Int
urcrsResponseStatus = lens _urcrsResponseStatus (\ s a -> s{_urcrsResponseStatus = a})

instance NFData UpdateRuntimeConfigurationResponse
         where

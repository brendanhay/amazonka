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
-- Module      : Network.AWS.GameLift.DescribeRuntimeConfiguration
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the current run-time configuration for the specified fleet. The run-time configuration tells Amazon GameLift how to launch server processes on instances in the fleet.
--
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
module Network.AWS.GameLift.DescribeRuntimeConfiguration
    (
    -- * Creating a Request
      describeRuntimeConfiguration
    , DescribeRuntimeConfiguration
    -- * Request Lenses
    , drcFleetId

    -- * Destructuring the Response
    , describeRuntimeConfigurationResponse
    , DescribeRuntimeConfigurationResponse
    -- * Response Lenses
    , drcrsRuntimeConfiguration
    , drcrsResponseStatus
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
-- /See:/ 'describeRuntimeConfiguration' smart constructor.
newtype DescribeRuntimeConfiguration = DescribeRuntimeConfiguration'
  { _drcFleetId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeRuntimeConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drcFleetId' - Unique identifier for a fleet to get the run-time configuration for.
describeRuntimeConfiguration
    :: Text -- ^ 'drcFleetId'
    -> DescribeRuntimeConfiguration
describeRuntimeConfiguration pFleetId_ =
  DescribeRuntimeConfiguration' {_drcFleetId = pFleetId_}


-- | Unique identifier for a fleet to get the run-time configuration for.
drcFleetId :: Lens' DescribeRuntimeConfiguration Text
drcFleetId = lens _drcFleetId (\ s a -> s{_drcFleetId = a})

instance AWSRequest DescribeRuntimeConfiguration
         where
        type Rs DescribeRuntimeConfiguration =
             DescribeRuntimeConfigurationResponse
        request = postJSON gameLift
        response
          = receiveJSON
              (\ s h x ->
                 DescribeRuntimeConfigurationResponse' <$>
                   (x .?> "RuntimeConfiguration") <*>
                     (pure (fromEnum s)))

instance Hashable DescribeRuntimeConfiguration where

instance NFData DescribeRuntimeConfiguration where

instance ToHeaders DescribeRuntimeConfiguration where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("GameLift.DescribeRuntimeConfiguration" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeRuntimeConfiguration where
        toJSON DescribeRuntimeConfiguration'{..}
          = object
              (catMaybes [Just ("FleetId" .= _drcFleetId)])

instance ToPath DescribeRuntimeConfiguration where
        toPath = const "/"

instance ToQuery DescribeRuntimeConfiguration where
        toQuery = const mempty

-- | Represents the returned data in response to a request action.
--
--
--
-- /See:/ 'describeRuntimeConfigurationResponse' smart constructor.
data DescribeRuntimeConfigurationResponse = DescribeRuntimeConfigurationResponse'
  { _drcrsRuntimeConfiguration :: !(Maybe RuntimeConfiguration)
  , _drcrsResponseStatus       :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeRuntimeConfigurationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drcrsRuntimeConfiguration' - Instructions describing how server processes should be launched and maintained on each instance in the fleet.
--
-- * 'drcrsResponseStatus' - -- | The response status code.
describeRuntimeConfigurationResponse
    :: Int -- ^ 'drcrsResponseStatus'
    -> DescribeRuntimeConfigurationResponse
describeRuntimeConfigurationResponse pResponseStatus_ =
  DescribeRuntimeConfigurationResponse'
    { _drcrsRuntimeConfiguration = Nothing
    , _drcrsResponseStatus = pResponseStatus_
    }


-- | Instructions describing how server processes should be launched and maintained on each instance in the fleet.
drcrsRuntimeConfiguration :: Lens' DescribeRuntimeConfigurationResponse (Maybe RuntimeConfiguration)
drcrsRuntimeConfiguration = lens _drcrsRuntimeConfiguration (\ s a -> s{_drcrsRuntimeConfiguration = a})

-- | -- | The response status code.
drcrsResponseStatus :: Lens' DescribeRuntimeConfigurationResponse Int
drcrsResponseStatus = lens _drcrsResponseStatus (\ s a -> s{_drcrsResponseStatus = a})

instance NFData DescribeRuntimeConfigurationResponse
         where

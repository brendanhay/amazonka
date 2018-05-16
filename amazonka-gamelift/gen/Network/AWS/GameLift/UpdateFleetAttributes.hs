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
-- Module      : Network.AWS.GameLift.UpdateFleetAttributes
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates fleet properties, including name and description, for a fleet. To update metadata, specify the fleet ID and the property values that you want to change. If successful, the fleet ID for the updated fleet is returned.
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
module Network.AWS.GameLift.UpdateFleetAttributes
    (
    -- * Creating a Request
      updateFleetAttributes
    , UpdateFleetAttributes
    -- * Request Lenses
    , ufaNewGameSessionProtectionPolicy
    , ufaName
    , ufaMetricGroups
    , ufaDescription
    , ufaResourceCreationLimitPolicy
    , ufaFleetId

    -- * Destructuring the Response
    , updateFleetAttributesResponse
    , UpdateFleetAttributesResponse
    -- * Response Lenses
    , ufarsFleetId
    , ufarsResponseStatus
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
-- /See:/ 'updateFleetAttributes' smart constructor.
data UpdateFleetAttributes = UpdateFleetAttributes'
  { _ufaNewGameSessionProtectionPolicy :: !(Maybe ProtectionPolicy)
  , _ufaName                           :: !(Maybe Text)
  , _ufaMetricGroups                   :: !(Maybe [Text])
  , _ufaDescription                    :: !(Maybe Text)
  , _ufaResourceCreationLimitPolicy    :: !(Maybe ResourceCreationLimitPolicy)
  , _ufaFleetId                        :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateFleetAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ufaNewGameSessionProtectionPolicy' - Game session protection policy to apply to all new instances created in this fleet. Instances that already exist are not affected. You can set protection for individual instances using 'UpdateGameSession' .     * __NoProtection__ -- The game session can be terminated during a scale-down event.     * __FullProtection__ -- If the game session is in an @ACTIVE@ status, it cannot be terminated during a scale-down event.
--
-- * 'ufaName' - Descriptive label that is associated with a fleet. Fleet names do not need to be unique.
--
-- * 'ufaMetricGroups' - Names of metric groups to include this fleet in. Amazon CloudWatch uses a fleet metric group is to aggregate metrics from multiple fleets. Use an existing metric group name to add this fleet to the group. Or use a new name to create a new metric group. A fleet can only be included in one metric group at a time.
--
-- * 'ufaDescription' - Human-readable description of a fleet.
--
-- * 'ufaResourceCreationLimitPolicy' - Policy that limits the number of game sessions an individual player can create over a span of time.
--
-- * 'ufaFleetId' - Unique identifier for a fleet to update attribute metadata for.
updateFleetAttributes
    :: Text -- ^ 'ufaFleetId'
    -> UpdateFleetAttributes
updateFleetAttributes pFleetId_ =
  UpdateFleetAttributes'
    { _ufaNewGameSessionProtectionPolicy = Nothing
    , _ufaName = Nothing
    , _ufaMetricGroups = Nothing
    , _ufaDescription = Nothing
    , _ufaResourceCreationLimitPolicy = Nothing
    , _ufaFleetId = pFleetId_
    }


-- | Game session protection policy to apply to all new instances created in this fleet. Instances that already exist are not affected. You can set protection for individual instances using 'UpdateGameSession' .     * __NoProtection__ -- The game session can be terminated during a scale-down event.     * __FullProtection__ -- If the game session is in an @ACTIVE@ status, it cannot be terminated during a scale-down event.
ufaNewGameSessionProtectionPolicy :: Lens' UpdateFleetAttributes (Maybe ProtectionPolicy)
ufaNewGameSessionProtectionPolicy = lens _ufaNewGameSessionProtectionPolicy (\ s a -> s{_ufaNewGameSessionProtectionPolicy = a})

-- | Descriptive label that is associated with a fleet. Fleet names do not need to be unique.
ufaName :: Lens' UpdateFleetAttributes (Maybe Text)
ufaName = lens _ufaName (\ s a -> s{_ufaName = a})

-- | Names of metric groups to include this fleet in. Amazon CloudWatch uses a fleet metric group is to aggregate metrics from multiple fleets. Use an existing metric group name to add this fleet to the group. Or use a new name to create a new metric group. A fleet can only be included in one metric group at a time.
ufaMetricGroups :: Lens' UpdateFleetAttributes [Text]
ufaMetricGroups = lens _ufaMetricGroups (\ s a -> s{_ufaMetricGroups = a}) . _Default . _Coerce

-- | Human-readable description of a fleet.
ufaDescription :: Lens' UpdateFleetAttributes (Maybe Text)
ufaDescription = lens _ufaDescription (\ s a -> s{_ufaDescription = a})

-- | Policy that limits the number of game sessions an individual player can create over a span of time.
ufaResourceCreationLimitPolicy :: Lens' UpdateFleetAttributes (Maybe ResourceCreationLimitPolicy)
ufaResourceCreationLimitPolicy = lens _ufaResourceCreationLimitPolicy (\ s a -> s{_ufaResourceCreationLimitPolicy = a})

-- | Unique identifier for a fleet to update attribute metadata for.
ufaFleetId :: Lens' UpdateFleetAttributes Text
ufaFleetId = lens _ufaFleetId (\ s a -> s{_ufaFleetId = a})

instance AWSRequest UpdateFleetAttributes where
        type Rs UpdateFleetAttributes =
             UpdateFleetAttributesResponse
        request = postJSON gameLift
        response
          = receiveJSON
              (\ s h x ->
                 UpdateFleetAttributesResponse' <$>
                   (x .?> "FleetId") <*> (pure (fromEnum s)))

instance Hashable UpdateFleetAttributes where

instance NFData UpdateFleetAttributes where

instance ToHeaders UpdateFleetAttributes where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("GameLift.UpdateFleetAttributes" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateFleetAttributes where
        toJSON UpdateFleetAttributes'{..}
          = object
              (catMaybes
                 [("NewGameSessionProtectionPolicy" .=) <$>
                    _ufaNewGameSessionProtectionPolicy,
                  ("Name" .=) <$> _ufaName,
                  ("MetricGroups" .=) <$> _ufaMetricGroups,
                  ("Description" .=) <$> _ufaDescription,
                  ("ResourceCreationLimitPolicy" .=) <$>
                    _ufaResourceCreationLimitPolicy,
                  Just ("FleetId" .= _ufaFleetId)])

instance ToPath UpdateFleetAttributes where
        toPath = const "/"

instance ToQuery UpdateFleetAttributes where
        toQuery = const mempty

-- | Represents the returned data in response to a request action.
--
--
--
-- /See:/ 'updateFleetAttributesResponse' smart constructor.
data UpdateFleetAttributesResponse = UpdateFleetAttributesResponse'
  { _ufarsFleetId        :: !(Maybe Text)
  , _ufarsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateFleetAttributesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ufarsFleetId' - Unique identifier for a fleet that was updated.
--
-- * 'ufarsResponseStatus' - -- | The response status code.
updateFleetAttributesResponse
    :: Int -- ^ 'ufarsResponseStatus'
    -> UpdateFleetAttributesResponse
updateFleetAttributesResponse pResponseStatus_ =
  UpdateFleetAttributesResponse'
    {_ufarsFleetId = Nothing, _ufarsResponseStatus = pResponseStatus_}


-- | Unique identifier for a fleet that was updated.
ufarsFleetId :: Lens' UpdateFleetAttributesResponse (Maybe Text)
ufarsFleetId = lens _ufarsFleetId (\ s a -> s{_ufarsFleetId = a})

-- | -- | The response status code.
ufarsResponseStatus :: Lens' UpdateFleetAttributesResponse Int
ufarsResponseStatus = lens _ufarsResponseStatus (\ s a -> s{_ufarsResponseStatus = a})

instance NFData UpdateFleetAttributesResponse where

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
-- Module      : Network.AWS.GameLift.StartFleetActions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resumes activity on a fleet that was suspended with 'StopFleetActions' . Currently, this operation is used to restart a fleet's auto-scaling activity.
--
--
-- To start fleet actions, specify the fleet ID and the type of actions to restart. When auto-scaling fleet actions are restarted, Amazon GameLift once again initiates scaling events as triggered by the fleet's scaling policies. If actions on the fleet were never stopped, this operation will have no effect. You can view a fleet's stopped actions using 'DescribeFleetAttributes' .
--
-- Operations related to fleet capacity scaling include:
--
--     * 'DescribeFleetCapacity'
--
--     * 'UpdateFleetCapacity'
--
--     * 'DescribeEC2InstanceLimits'
--
--     * Manage scaling policies:
--
--     * 'PutScalingPolicy' (auto-scaling)
--
--     * 'DescribeScalingPolicies' (auto-scaling)
--
--     * 'DeleteScalingPolicy' (auto-scaling)
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
module Network.AWS.GameLift.StartFleetActions
    (
    -- * Creating a Request
      startFleetActions
    , StartFleetActions
    -- * Request Lenses
    , sfaFleetId
    , sfaActions

    -- * Destructuring the Response
    , startFleetActionsResponse
    , StartFleetActionsResponse
    -- * Response Lenses
    , sfarsResponseStatus
    ) where

import Network.AWS.GameLift.Types
import Network.AWS.GameLift.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'startFleetActions' smart constructor.
data StartFleetActions = StartFleetActions'
  { _sfaFleetId :: !Text
  , _sfaActions :: !(List1 FleetAction)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartFleetActions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sfaFleetId' - Unique identifier for a fleet
--
-- * 'sfaActions' - List of actions to restart on the fleet.
startFleetActions
    :: Text -- ^ 'sfaFleetId'
    -> NonEmpty FleetAction -- ^ 'sfaActions'
    -> StartFleetActions
startFleetActions pFleetId_ pActions_ =
  StartFleetActions' {_sfaFleetId = pFleetId_, _sfaActions = _List1 # pActions_}


-- | Unique identifier for a fleet
sfaFleetId :: Lens' StartFleetActions Text
sfaFleetId = lens _sfaFleetId (\ s a -> s{_sfaFleetId = a})

-- | List of actions to restart on the fleet.
sfaActions :: Lens' StartFleetActions (NonEmpty FleetAction)
sfaActions = lens _sfaActions (\ s a -> s{_sfaActions = a}) . _List1

instance AWSRequest StartFleetActions where
        type Rs StartFleetActions = StartFleetActionsResponse
        request = postJSON gameLift
        response
          = receiveEmpty
              (\ s h x ->
                 StartFleetActionsResponse' <$> (pure (fromEnum s)))

instance Hashable StartFleetActions where

instance NFData StartFleetActions where

instance ToHeaders StartFleetActions where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("GameLift.StartFleetActions" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StartFleetActions where
        toJSON StartFleetActions'{..}
          = object
              (catMaybes
                 [Just ("FleetId" .= _sfaFleetId),
                  Just ("Actions" .= _sfaActions)])

instance ToPath StartFleetActions where
        toPath = const "/"

instance ToQuery StartFleetActions where
        toQuery = const mempty

-- | /See:/ 'startFleetActionsResponse' smart constructor.
newtype StartFleetActionsResponse = StartFleetActionsResponse'
  { _sfarsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartFleetActionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sfarsResponseStatus' - -- | The response status code.
startFleetActionsResponse
    :: Int -- ^ 'sfarsResponseStatus'
    -> StartFleetActionsResponse
startFleetActionsResponse pResponseStatus_ =
  StartFleetActionsResponse' {_sfarsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
sfarsResponseStatus :: Lens' StartFleetActionsResponse Int
sfarsResponseStatus = lens _sfarsResponseStatus (\ s a -> s{_sfarsResponseStatus = a})

instance NFData StartFleetActionsResponse where

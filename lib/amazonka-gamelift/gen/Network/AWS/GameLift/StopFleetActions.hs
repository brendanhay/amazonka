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
-- Module      : Network.AWS.GameLift.StopFleetActions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Suspends activity on a fleet. Currently, this operation is used to stop a fleet's auto-scaling activity. It is used to temporarily stop scaling events triggered by the fleet's scaling policies. The policies can be retained and auto-scaling activity can be restarted using 'StartFleetActions' . You can view a fleet's stopped actions using 'DescribeFleetAttributes' .
--
--
-- To stop fleet actions, specify the fleet ID and the type of actions to suspend. When auto-scaling fleet actions are stopped, Amazon GameLift no longer initiates scaling events except to maintain the fleet's desired instances setting ('FleetCapacity' . Changes to the fleet's capacity must be done manually using 'UpdateFleetCapacity' .
--
module Network.AWS.GameLift.StopFleetActions
    (
    -- * Creating a Request
      stopFleetActions
    , StopFleetActions
    -- * Request Lenses
    , sFleetId
    , sActions

    -- * Destructuring the Response
    , stopFleetActionsResponse
    , StopFleetActionsResponse
    -- * Response Lenses
    , sfasrsResponseStatus
    ) where

import Network.AWS.GameLift.Types
import Network.AWS.GameLift.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'stopFleetActions' smart constructor.
data StopFleetActions = StopFleetActions'
  { _sFleetId :: !Text
  , _sActions :: !(List1 FleetAction)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StopFleetActions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sFleetId' - Unique identifier for a fleet
--
-- * 'sActions' - List of actions to suspend on the fleet.
stopFleetActions
    :: Text -- ^ 'sFleetId'
    -> NonEmpty FleetAction -- ^ 'sActions'
    -> StopFleetActions
stopFleetActions pFleetId_ pActions_ =
  StopFleetActions' {_sFleetId = pFleetId_, _sActions = _List1 # pActions_}


-- | Unique identifier for a fleet
sFleetId :: Lens' StopFleetActions Text
sFleetId = lens _sFleetId (\ s a -> s{_sFleetId = a})

-- | List of actions to suspend on the fleet.
sActions :: Lens' StopFleetActions (NonEmpty FleetAction)
sActions = lens _sActions (\ s a -> s{_sActions = a}) . _List1

instance AWSRequest StopFleetActions where
        type Rs StopFleetActions = StopFleetActionsResponse
        request = postJSON gameLift
        response
          = receiveEmpty
              (\ s h x ->
                 StopFleetActionsResponse' <$> (pure (fromEnum s)))

instance Hashable StopFleetActions where

instance NFData StopFleetActions where

instance ToHeaders StopFleetActions where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("GameLift.StopFleetActions" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StopFleetActions where
        toJSON StopFleetActions'{..}
          = object
              (catMaybes
                 [Just ("FleetId" .= _sFleetId),
                  Just ("Actions" .= _sActions)])

instance ToPath StopFleetActions where
        toPath = const "/"

instance ToQuery StopFleetActions where
        toQuery = const mempty

-- | /See:/ 'stopFleetActionsResponse' smart constructor.
newtype StopFleetActionsResponse = StopFleetActionsResponse'
  { _sfasrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StopFleetActionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sfasrsResponseStatus' - -- | The response status code.
stopFleetActionsResponse
    :: Int -- ^ 'sfasrsResponseStatus'
    -> StopFleetActionsResponse
stopFleetActionsResponse pResponseStatus_ =
  StopFleetActionsResponse' {_sfasrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
sfasrsResponseStatus :: Lens' StopFleetActionsResponse Int
sfasrsResponseStatus = lens _sfasrsResponseStatus (\ s a -> s{_sfasrsResponseStatus = a})

instance NFData StopFleetActionsResponse where

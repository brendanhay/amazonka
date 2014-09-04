{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudWatch.V2010_08_01.SetAlarmState
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Temporarily sets the state of an alarm. When the updated StateValue differs
-- from the previous value, the action configured for the appropriate state is
-- invoked. This is not a permanent change. The next periodic alarm check (in
-- about a minute) will set the alarm to its actual state.
module Network.AWS.CloudWatch.V2010_08_01.SetAlarmState
    (
    -- * Request
      SetAlarmState
    -- ** Request constructor
    , mkSetAlarmStateInput
    -- ** Request lenses
    , sasiAlarmName
    , sasiStateValue
    , sasiStateReason
    , sasiStateReasonData

    -- * Response
    , SetAlarmStateResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.CloudWatch.V2010_08_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'SetAlarmState' request.
mkSetAlarmStateInput :: Text -- ^ 'sasiAlarmName'
                     -> StateValue -- ^ 'sasiStateValue'
                     -> Text -- ^ 'sasiStateReason'
                     -> SetAlarmState
mkSetAlarmStateInput p1 p2 p3 = SetAlarmState
    { _sasiAlarmName = p1
    , _sasiStateValue = p2
    , _sasiStateReason = p3
    , _sasiStateReasonData = Nothing
    }
{-# INLINE mkSetAlarmStateInput #-}

data SetAlarmState = SetAlarmState
    { _sasiAlarmName :: Text
      -- ^ The descriptive name for the alarm. This name must be unique
      -- within the user's AWS account. The maximum length is 255
      -- characters.
    , _sasiStateValue :: StateValue
      -- ^ The value of the state.
    , _sasiStateReason :: Text
      -- ^ The reason that this alarm is set to this specific state (in
      -- human-readable text format).
    , _sasiStateReasonData :: Maybe Text
      -- ^ The reason that this alarm is set to this specific state (in
      -- machine-readable JSON format).
    } deriving (Show, Generic)

-- | The descriptive name for the alarm. This name must be unique within the
-- user's AWS account. The maximum length is 255 characters.
sasiAlarmName :: Lens' SetAlarmState (Text)
sasiAlarmName = lens _sasiAlarmName (\s a -> s { _sasiAlarmName = a })
{-# INLINE sasiAlarmName #-}

-- | The value of the state.
sasiStateValue :: Lens' SetAlarmState (StateValue)
sasiStateValue = lens _sasiStateValue (\s a -> s { _sasiStateValue = a })
{-# INLINE sasiStateValue #-}

-- | The reason that this alarm is set to this specific state (in human-readable
-- text format).
sasiStateReason :: Lens' SetAlarmState (Text)
sasiStateReason = lens _sasiStateReason (\s a -> s { _sasiStateReason = a })
{-# INLINE sasiStateReason #-}

-- | The reason that this alarm is set to this specific state (in
-- machine-readable JSON format).
sasiStateReasonData :: Lens' SetAlarmState (Maybe Text)
sasiStateReasonData = lens _sasiStateReasonData (\s a -> s { _sasiStateReasonData = a })
{-# INLINE sasiStateReasonData #-}

instance ToQuery SetAlarmState where
    toQuery = genericQuery def

data SetAlarmStateResponse = SetAlarmStateResponse
    deriving (Eq, Show, Generic)

instance AWSRequest SetAlarmState where
    type Sv SetAlarmState = CloudWatch
    type Rs SetAlarmState = SetAlarmStateResponse

    request = post "SetAlarmState"
    response _ = nullaryResponse SetAlarmStateResponse

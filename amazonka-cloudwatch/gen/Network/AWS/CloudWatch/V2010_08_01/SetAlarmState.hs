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
    , setAlarmState
    -- ** Request lenses
    , sasiAlarmName
    , sasiStateReason
    , sasiStateValue
    , sasiStateReasonData

    -- * Response
    , SetAlarmStateResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.CloudWatch.V2010_08_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'SetAlarmState' request.
setAlarmState :: Text -- ^ 'sasiAlarmName'
              -> Text -- ^ 'sasiStateReason'
              -> StateValue -- ^ 'sasiStateValue'
              -> SetAlarmState
setAlarmState p1 p2 p3 = SetAlarmState
    { _sasiAlarmName = p1
    , _sasiStateReason = p2
    , _sasiStateValue = p3
    , _sasiStateReasonData = Nothing
    }

data SetAlarmState = SetAlarmState
    { _sasiAlarmName :: Text
      -- ^ The descriptive name for the alarm. This name must be unique
      -- within the user's AWS account. The maximum length is 255
      -- characters.
    , _sasiStateReason :: Text
      -- ^ The reason that this alarm is set to this specific state (in
      -- human-readable text format).
    , _sasiStateValue :: StateValue
      -- ^ The value of the state.
    , _sasiStateReasonData :: Maybe Text
      -- ^ The reason that this alarm is set to this specific state (in
      -- machine-readable JSON format).
    } deriving (Show, Generic)

-- | The descriptive name for the alarm. This name must be unique within the
-- user's AWS account. The maximum length is 255 characters.
sasiAlarmName
    :: Functor f
    => (Text
    -> f (Text))
    -> SetAlarmState
    -> f SetAlarmState
sasiAlarmName f x =
    (\y -> x { _sasiAlarmName = y })
       <$> f (_sasiAlarmName x)
{-# INLINE sasiAlarmName #-}

-- | The reason that this alarm is set to this specific state (in human-readable
-- text format).
sasiStateReason
    :: Functor f
    => (Text
    -> f (Text))
    -> SetAlarmState
    -> f SetAlarmState
sasiStateReason f x =
    (\y -> x { _sasiStateReason = y })
       <$> f (_sasiStateReason x)
{-# INLINE sasiStateReason #-}

-- | The value of the state.
sasiStateValue
    :: Functor f
    => (StateValue
    -> f (StateValue))
    -> SetAlarmState
    -> f SetAlarmState
sasiStateValue f x =
    (\y -> x { _sasiStateValue = y })
       <$> f (_sasiStateValue x)
{-# INLINE sasiStateValue #-}

-- | The reason that this alarm is set to this specific state (in
-- machine-readable JSON format).
sasiStateReasonData
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> SetAlarmState
    -> f SetAlarmState
sasiStateReasonData f x =
    (\y -> x { _sasiStateReasonData = y })
       <$> f (_sasiStateReasonData x)
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

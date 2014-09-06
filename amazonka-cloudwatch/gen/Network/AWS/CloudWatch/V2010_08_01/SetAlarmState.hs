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
    , mkSetAlarmState
    -- ** Request lenses
    , sasAlarmName
    , sasStateValue
    , sasStateReason
    , sasStateReasonData

    -- * Response
    , SetAlarmStateResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.CloudWatch.V2010_08_01.Types
import Network.AWS.Prelude

-- | 
data SetAlarmState = SetAlarmState
    { _sasAlarmName :: Text
    , _sasStateValue :: StateValue
    , _sasStateReason :: Text
    , _sasStateReasonData :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'SetAlarmState' request.
mkSetAlarmState :: Text -- ^ 'sasAlarmName'
                -> StateValue -- ^ 'sasStateValue'
                -> Text -- ^ 'sasStateReason'
                -> SetAlarmState
mkSetAlarmState p1 p2 p3 = SetAlarmState
    { _sasAlarmName = p1
    , _sasStateValue = p2
    , _sasStateReason = p3
    , _sasStateReasonData = Nothing
    }
{-# INLINE mkSetAlarmState #-}

-- | The descriptive name for the alarm. This name must be unique within the
-- user's AWS account. The maximum length is 255 characters.
sasAlarmName :: Lens' SetAlarmState Text
sasAlarmName = lens _sasAlarmName (\s a -> s { _sasAlarmName = a })
{-# INLINE sasAlarmName #-}

-- | The value of the state.
sasStateValue :: Lens' SetAlarmState StateValue
sasStateValue = lens _sasStateValue (\s a -> s { _sasStateValue = a })
{-# INLINE sasStateValue #-}

-- | The reason that this alarm is set to this specific state (in human-readable
-- text format).
sasStateReason :: Lens' SetAlarmState Text
sasStateReason = lens _sasStateReason (\s a -> s { _sasStateReason = a })
{-# INLINE sasStateReason #-}

-- | The reason that this alarm is set to this specific state (in
-- machine-readable JSON format).
sasStateReasonData :: Lens' SetAlarmState (Maybe Text)
sasStateReasonData =
    lens _sasStateReasonData (\s a -> s { _sasStateReasonData = a })
{-# INLINE sasStateReasonData #-}

instance ToQuery SetAlarmState where
    toQuery = genericQuery def

data SetAlarmStateResponse = SetAlarmStateResponse
    deriving (Eq, Show, Generic)

instance AWSRequest SetAlarmState where
    type Sv SetAlarmState = CloudWatch
    type Rs SetAlarmState = SetAlarmStateResponse

    request = post "SetAlarmState"
    response _ = nullaryResponse SetAlarmStateResponse

{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudWatch.SetAlarmState
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
--
-- <http://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_SetAlarmState.html>
module Network.AWS.CloudWatch.SetAlarmState
    (
    -- * Request
      SetAlarmState
    -- ** Request constructor
    , setAlarmState
    -- ** Request lenses
    , sasAlarmName
    , sasStateReason
    , sasStateReasonData
    , sasStateValue

    -- * Response
    , SetAlarmStateResponse
    -- ** Response constructor
    , setAlarmStateResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.CloudWatch.Types
import qualified GHC.Exts

data SetAlarmState = SetAlarmState
    { _sasAlarmName       :: Text
    , _sasStateReason     :: Text
    , _sasStateReasonData :: Maybe Text
    , _sasStateValue      :: Text
    } deriving (Eq, Ord, Show)

-- | 'SetAlarmState' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sasAlarmName' @::@ 'Text'
--
-- * 'sasStateReason' @::@ 'Text'
--
-- * 'sasStateReasonData' @::@ 'Maybe' 'Text'
--
-- * 'sasStateValue' @::@ 'Text'
--
setAlarmState :: Text -- ^ 'sasAlarmName'
              -> Text -- ^ 'sasStateValue'
              -> Text -- ^ 'sasStateReason'
              -> SetAlarmState
setAlarmState p1 p2 p3 = SetAlarmState
    { _sasAlarmName       = p1
    , _sasStateValue      = p2
    , _sasStateReason     = p3
    , _sasStateReasonData = Nothing
    }

-- | The descriptive name for the alarm. This name must be unique within the
-- user's AWS account. The maximum length is 255 characters.
sasAlarmName :: Lens' SetAlarmState Text
sasAlarmName = lens _sasAlarmName (\s a -> s { _sasAlarmName = a })

-- | The reason that this alarm is set to this specific state (in
-- human-readable text format).
sasStateReason :: Lens' SetAlarmState Text
sasStateReason = lens _sasStateReason (\s a -> s { _sasStateReason = a })

-- | The reason that this alarm is set to this specific state (in
-- machine-readable JSON format).
sasStateReasonData :: Lens' SetAlarmState (Maybe Text)
sasStateReasonData =
    lens _sasStateReasonData (\s a -> s { _sasStateReasonData = a })

-- | The value of the state.
sasStateValue :: Lens' SetAlarmState Text
sasStateValue = lens _sasStateValue (\s a -> s { _sasStateValue = a })

data SetAlarmStateResponse = SetAlarmStateResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'SetAlarmStateResponse' constructor.
setAlarmStateResponse :: SetAlarmStateResponse
setAlarmStateResponse = SetAlarmStateResponse

instance ToPath SetAlarmState where
    toPath = const "/"

instance ToQuery SetAlarmState where
    toQuery SetAlarmState{..} = mconcat
        [ "AlarmName"       =? _sasAlarmName
        , "StateReason"     =? _sasStateReason
        , "StateReasonData" =? _sasStateReasonData
        , "StateValue"      =? _sasStateValue
        ]

instance ToHeaders SetAlarmState

instance AWSRequest SetAlarmState where
    type Sv SetAlarmState = CloudWatch
    type Rs SetAlarmState = SetAlarmStateResponse

    request  = post "SetAlarmState"
    response = nullResponse SetAlarmStateResponse


Some kind of operator / class to check the types whether to continue?

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

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
module Network.AWS.CloudWatch.SetAlarmState
    (
    -- * Request
      SetAlarmStateInput
    -- ** Request constructor
    , setAlarmState
    -- ** Request lenses
    , sasiAlarmName
    , sasiStateReason
    , sasiStateReasonData
    , sasiStateValue

    -- * Response
    , SetAlarmStateResponse
    -- ** Response constructor
    , setAlarmStateResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.CloudWatch.Types

data SetAlarmStateInput = SetAlarmStateInput
    { _sasiAlarmName       :: Text
    , _sasiStateReason     :: Text
    , _sasiStateReasonData :: Maybe Text
    , _sasiStateValue      :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'SetAlarmStateInput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sasiAlarmName' @::@ 'Text'
--
-- * 'sasiStateReason' @::@ 'Text'
--
-- * 'sasiStateReasonData' @::@ 'Maybe' 'Text'
--
-- * 'sasiStateValue' @::@ 'Text'
--
setAlarmState :: Text -- ^ 'sasiAlarmName'
              -> Text -- ^ 'sasiStateValue'
              -> Text -- ^ 'sasiStateReason'
              -> SetAlarmStateInput
setAlarmState p1 p2 p3 = SetAlarmStateInput
    { _sasiAlarmName       = p1
    , _sasiStateValue      = p2
    , _sasiStateReason     = p3
    , _sasiStateReasonData = Nothing
    }

-- | The descriptive name for the alarm. This name must be unique within the
-- user's AWS account. The maximum length is 255 characters.
sasiAlarmName :: Lens' SetAlarmStateInput Text
sasiAlarmName = lens _sasiAlarmName (\s a -> s { _sasiAlarmName = a })

-- | The reason that this alarm is set to this specific state (in
-- human-readable text format).
sasiStateReason :: Lens' SetAlarmStateInput Text
sasiStateReason = lens _sasiStateReason (\s a -> s { _sasiStateReason = a })

-- | The reason that this alarm is set to this specific state (in
-- machine-readable JSON format).
sasiStateReasonData :: Lens' SetAlarmStateInput (Maybe Text)
sasiStateReasonData =
    lens _sasiStateReasonData (\s a -> s { _sasiStateReasonData = a })

-- | The value of the state.
sasiStateValue :: Lens' SetAlarmStateInput Text
sasiStateValue = lens _sasiStateValue (\s a -> s { _sasiStateValue = a })

instance ToPath SetAlarmStateInput where
    toPath = const "/"

instance ToQuery SetAlarmStateInput

data SetAlarmStateResponse = SetAlarmStateResponse

-- | 'SetAlarmStateResponse' constructor.
setAlarmStateResponse :: SetAlarmStateResponse
setAlarmStateResponse = SetAlarmStateResponse

instance AWSRequest SetAlarmStateInput where
    type Sv SetAlarmStateInput = CloudWatch
    type Rs SetAlarmStateInput = SetAlarmStateResponse

    request  = post "SetAlarmState"
    response = const (nullaryResponse SetAlarmStateResponse)

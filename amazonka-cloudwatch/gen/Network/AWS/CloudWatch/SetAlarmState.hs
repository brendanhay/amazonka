{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CloudWatch.SetAlarmState
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Temporarily sets the state of an alarm. When the updated @StateValue@
-- differs from the previous value, the action configured for the
-- appropriate state is invoked. This is not a permanent change. The next
-- periodic alarm check (in about a minute) will set the alarm to its
-- actual state.
--
-- <http://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_SetAlarmState.html>
module Network.AWS.CloudWatch.SetAlarmState
    (
    -- * Request
      SetAlarmState
    -- ** Request constructor
    , setAlarmState
    -- ** Request lenses
    , sasStateReasonData
    , sasAlarmName
    , sasStateValue
    , sasStateReason

    -- * Response
    , SetAlarmStateResponse
    -- ** Response constructor
    , setAlarmStateResponse
    ) where

import Network.AWS.CloudWatch.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'setAlarmState' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sasStateReasonData'
--
-- * 'sasAlarmName'
--
-- * 'sasStateValue'
--
-- * 'sasStateReason'
data SetAlarmState = SetAlarmState'{_sasStateReasonData :: Maybe Text, _sasAlarmName :: Text, _sasStateValue :: StateValue, _sasStateReason :: Text} deriving (Eq, Read, Show)

-- | 'SetAlarmState' smart constructor.
setAlarmState :: Text -> StateValue -> Text -> SetAlarmState
setAlarmState pAlarmName pStateValue pStateReason = SetAlarmState'{_sasStateReasonData = Nothing, _sasAlarmName = pAlarmName, _sasStateValue = pStateValue, _sasStateReason = pStateReason};

-- | The reason that this alarm is set to this specific state (in
-- machine-readable JSON format)
sasStateReasonData :: Lens' SetAlarmState (Maybe Text)
sasStateReasonData = lens _sasStateReasonData (\ s a -> s{_sasStateReasonData = a});

-- | The descriptive name for the alarm. This name must be unique within the
-- user\'s AWS account. The maximum length is 255 characters.
sasAlarmName :: Lens' SetAlarmState Text
sasAlarmName = lens _sasAlarmName (\ s a -> s{_sasAlarmName = a});

-- | The value of the state.
sasStateValue :: Lens' SetAlarmState StateValue
sasStateValue = lens _sasStateValue (\ s a -> s{_sasStateValue = a});

-- | The reason that this alarm is set to this specific state (in
-- human-readable text format)
sasStateReason :: Lens' SetAlarmState Text
sasStateReason = lens _sasStateReason (\ s a -> s{_sasStateReason = a});

instance AWSPager A where
        page rq rs
          | stop True = Nothing
          | otherwise = Just

instance AWSRequest SetAlarmState where
        type Sv SetAlarmState = CloudWatch
        type Rs SetAlarmState = SetAlarmStateResponse
        request = post
        response = receiveNull SetAlarmStateResponse'

instance ToHeaders SetAlarmState where
        toHeaders = const mempty

instance ToPath SetAlarmState where
        toPath = const "/"

instance ToQuery SetAlarmState where
        toQuery SetAlarmState'{..}
          = mconcat
              ["Action" =: ("SetAlarmState" :: ByteString),
               "Version" =: ("2010-08-01" :: ByteString),
               "StateReasonData" =: _sasStateReasonData,
               "AlarmName" =: _sasAlarmName,
               "StateValue" =: _sasStateValue,
               "StateReason" =: _sasStateReason]

-- | /See:/ 'setAlarmStateResponse' smart constructor.
data SetAlarmStateResponse = SetAlarmStateResponse' deriving (Eq, Read, Show)

-- | 'SetAlarmStateResponse' smart constructor.
setAlarmStateResponse :: SetAlarmStateResponse
setAlarmStateResponse = SetAlarmStateResponse';

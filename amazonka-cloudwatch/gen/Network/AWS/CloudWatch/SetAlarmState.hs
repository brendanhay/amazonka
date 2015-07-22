{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.SetAlarmState
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Temporarily sets the state of an alarm. When the updated @StateValue@
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
    , sasrqStateReasonData
    , sasrqAlarmName
    , sasrqStateValue
    , sasrqStateReason

    -- * Response
    , SetAlarmStateResponse
    -- ** Response constructor
    , setAlarmStateResponse
    ) where

import           Network.AWS.CloudWatch.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'setAlarmState' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sasrqStateReasonData'
--
-- * 'sasrqAlarmName'
--
-- * 'sasrqStateValue'
--
-- * 'sasrqStateReason'
data SetAlarmState = SetAlarmState'
    { _sasrqStateReasonData :: !(Maybe Text)
    , _sasrqAlarmName       :: !Text
    , _sasrqStateValue      :: !StateValue
    , _sasrqStateReason     :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'SetAlarmState' smart constructor.
setAlarmState :: Text -> StateValue -> Text -> SetAlarmState
setAlarmState pAlarmName pStateValue pStateReason =
    SetAlarmState'
    { _sasrqStateReasonData = Nothing
    , _sasrqAlarmName = pAlarmName
    , _sasrqStateValue = pStateValue
    , _sasrqStateReason = pStateReason
    }

-- | The reason that this alarm is set to this specific state (in
-- machine-readable JSON format)
sasrqStateReasonData :: Lens' SetAlarmState (Maybe Text)
sasrqStateReasonData = lens _sasrqStateReasonData (\ s a -> s{_sasrqStateReasonData = a});

-- | The descriptive name for the alarm. This name must be unique within the
-- user\'s AWS account. The maximum length is 255 characters.
sasrqAlarmName :: Lens' SetAlarmState Text
sasrqAlarmName = lens _sasrqAlarmName (\ s a -> s{_sasrqAlarmName = a});

-- | The value of the state.
sasrqStateValue :: Lens' SetAlarmState StateValue
sasrqStateValue = lens _sasrqStateValue (\ s a -> s{_sasrqStateValue = a});

-- | The reason that this alarm is set to this specific state (in
-- human-readable text format)
sasrqStateReason :: Lens' SetAlarmState Text
sasrqStateReason = lens _sasrqStateReason (\ s a -> s{_sasrqStateReason = a});

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
               "StateReasonData" =: _sasrqStateReasonData,
               "AlarmName" =: _sasrqAlarmName,
               "StateValue" =: _sasrqStateValue,
               "StateReason" =: _sasrqStateReason]

-- | /See:/ 'setAlarmStateResponse' smart constructor.
data SetAlarmStateResponse =
    SetAlarmStateResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'SetAlarmStateResponse' smart constructor.
setAlarmStateResponse :: SetAlarmStateResponse
setAlarmStateResponse = SetAlarmStateResponse'

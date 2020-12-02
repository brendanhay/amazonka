{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.AssessmentRunNotification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.AssessmentRunNotification where

import Network.AWS.Inspector.Types.AssessmentRunNotificationSNSStatusCode
import Network.AWS.Inspector.Types.InspectorEvent
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Used as one of the elements of the 'AssessmentRun' data type.
--
--
--
-- /See:/ 'assessmentRunNotification' smart constructor.
data AssessmentRunNotification = AssessmentRunNotification'
  { _arnSnsTopicARN ::
      !(Maybe Text),
    _arnSnsPublishStatusCode ::
      !( Maybe
           AssessmentRunNotificationSNSStatusCode
       ),
    _arnMessage :: !(Maybe Text),
    _arnDate :: !POSIX,
    _arnEvent :: !InspectorEvent,
    _arnError :: !Bool
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AssessmentRunNotification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'arnSnsTopicARN' - The SNS topic to which the SNS notification is sent.
--
-- * 'arnSnsPublishStatusCode' - The status code of the SNS notification.
--
-- * 'arnMessage' - The message included in the notification.
--
-- * 'arnDate' - The date of the notification.
--
-- * 'arnEvent' - The event for which a notification is sent.
--
-- * 'arnError' - The Boolean value that specifies whether the notification represents an error.
assessmentRunNotification ::
  -- | 'arnDate'
  UTCTime ->
  -- | 'arnEvent'
  InspectorEvent ->
  -- | 'arnError'
  Bool ->
  AssessmentRunNotification
assessmentRunNotification pDate_ pEvent_ pError_ =
  AssessmentRunNotification'
    { _arnSnsTopicARN = Nothing,
      _arnSnsPublishStatusCode = Nothing,
      _arnMessage = Nothing,
      _arnDate = _Time # pDate_,
      _arnEvent = pEvent_,
      _arnError = pError_
    }

-- | The SNS topic to which the SNS notification is sent.
arnSnsTopicARN :: Lens' AssessmentRunNotification (Maybe Text)
arnSnsTopicARN = lens _arnSnsTopicARN (\s a -> s {_arnSnsTopicARN = a})

-- | The status code of the SNS notification.
arnSnsPublishStatusCode :: Lens' AssessmentRunNotification (Maybe AssessmentRunNotificationSNSStatusCode)
arnSnsPublishStatusCode = lens _arnSnsPublishStatusCode (\s a -> s {_arnSnsPublishStatusCode = a})

-- | The message included in the notification.
arnMessage :: Lens' AssessmentRunNotification (Maybe Text)
arnMessage = lens _arnMessage (\s a -> s {_arnMessage = a})

-- | The date of the notification.
arnDate :: Lens' AssessmentRunNotification UTCTime
arnDate = lens _arnDate (\s a -> s {_arnDate = a}) . _Time

-- | The event for which a notification is sent.
arnEvent :: Lens' AssessmentRunNotification InspectorEvent
arnEvent = lens _arnEvent (\s a -> s {_arnEvent = a})

-- | The Boolean value that specifies whether the notification represents an error.
arnError :: Lens' AssessmentRunNotification Bool
arnError = lens _arnError (\s a -> s {_arnError = a})

instance FromJSON AssessmentRunNotification where
  parseJSON =
    withObject
      "AssessmentRunNotification"
      ( \x ->
          AssessmentRunNotification'
            <$> (x .:? "snsTopicArn")
            <*> (x .:? "snsPublishStatusCode")
            <*> (x .:? "message")
            <*> (x .: "date")
            <*> (x .: "event")
            <*> (x .: "error")
      )

instance Hashable AssessmentRunNotification

instance NFData AssessmentRunNotification

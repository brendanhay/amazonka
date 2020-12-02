{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.PushNotificationTemplateResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.PushNotificationTemplateResponse where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types.APNSPushNotificationTemplate
import Network.AWS.Pinpoint.Types.AndroidPushNotificationTemplate
import Network.AWS.Pinpoint.Types.DefaultPushNotificationTemplate
import Network.AWS.Pinpoint.Types.TemplateType
import Network.AWS.Prelude

-- | Provides information about the content and settings for a message template that can be used in messages that are sent through a push notification channel.
--
--
--
-- /See:/ 'pushNotificationTemplateResponse' smart constructor.
data PushNotificationTemplateResponse = PushNotificationTemplateResponse'
  { _pntARN ::
      !(Maybe Text),
    _pntDefault ::
      !( Maybe
           DefaultPushNotificationTemplate
       ),
    _pntTemplateDescription ::
      !(Maybe Text),
    _pntGCM ::
      !( Maybe
           AndroidPushNotificationTemplate
       ),
    _pntAPNS ::
      !( Maybe
           APNSPushNotificationTemplate
       ),
    _pntDefaultSubstitutions ::
      !(Maybe Text),
    _pntVersion ::
      !(Maybe Text),
    _pntADM ::
      !( Maybe
           AndroidPushNotificationTemplate
       ),
    _pntBaidu ::
      !( Maybe
           AndroidPushNotificationTemplate
       ),
    _pntRecommenderId ::
      !(Maybe Text),
    _pntTags ::
      !( Maybe
           (Map Text (Text))
       ),
    _pntLastModifiedDate ::
      !Text,
    _pntCreationDate :: !Text,
    _pntTemplateType ::
      !TemplateType,
    _pntTemplateName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PushNotificationTemplateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pntARN' - The Amazon Resource Name (ARN) of the message template.
--
-- * 'pntDefault' - The default message template that's used for push notification channels.
--
-- * 'pntTemplateDescription' - The custom description of the message template.
--
-- * 'pntGCM' - The message template that's used for the GCM channel, which is used to send notifications through the Firebase Cloud Messaging (FCM), formerly Google Cloud Messaging (GCM), service. This message template overrides the default template for push notification channels (DefaultPushNotificationTemplate).
--
-- * 'pntAPNS' - The message template that's used for the APNs (Apple Push Notification service) channel. This message template overrides the default template for push notification channels (DefaultPushNotificationTemplate).
--
-- * 'pntDefaultSubstitutions' - The JSON object that specifies the default values that are used for message variables in the message template. This object is a set of key-value pairs. Each key defines a message variable in the template. The corresponding value defines the default value for that variable.
--
-- * 'pntVersion' - The unique identifier, as an integer, for the active version of the message template, or the version of the template that you specified by using the version parameter in your request.
--
-- * 'pntADM' - The message template that's used for the ADM (Amazon Device Messaging) channel. This message template overrides the default template for push notification channels (DefaultPushNotificationTemplate).
--
-- * 'pntBaidu' - The message template that's used for the Baidu (Baidu Cloud Push) channel. This message template overrides the default template for push notification channels (DefaultPushNotificationTemplate).
--
-- * 'pntRecommenderId' - The unique identifier for the recommender model that's used by the message template.
--
-- * 'pntTags' - A string-to-string map of key-value pairs that identifies the tags that are associated with the message template. Each tag consists of a required tag key and an associated tag value.
--
-- * 'pntLastModifiedDate' - The date, in ISO 8601 format, when the message template was last modified.
--
-- * 'pntCreationDate' - The date, in ISO 8601 format, when the message template was created.
--
-- * 'pntTemplateType' - The type of channel that the message template is designed for. For a push notification template, this value is PUSH.
--
-- * 'pntTemplateName' - The name of the message template.
pushNotificationTemplateResponse ::
  -- | 'pntLastModifiedDate'
  Text ->
  -- | 'pntCreationDate'
  Text ->
  -- | 'pntTemplateType'
  TemplateType ->
  -- | 'pntTemplateName'
  Text ->
  PushNotificationTemplateResponse
pushNotificationTemplateResponse
  pLastModifiedDate_
  pCreationDate_
  pTemplateType_
  pTemplateName_ =
    PushNotificationTemplateResponse'
      { _pntARN = Nothing,
        _pntDefault = Nothing,
        _pntTemplateDescription = Nothing,
        _pntGCM = Nothing,
        _pntAPNS = Nothing,
        _pntDefaultSubstitutions = Nothing,
        _pntVersion = Nothing,
        _pntADM = Nothing,
        _pntBaidu = Nothing,
        _pntRecommenderId = Nothing,
        _pntTags = Nothing,
        _pntLastModifiedDate = pLastModifiedDate_,
        _pntCreationDate = pCreationDate_,
        _pntTemplateType = pTemplateType_,
        _pntTemplateName = pTemplateName_
      }

-- | The Amazon Resource Name (ARN) of the message template.
pntARN :: Lens' PushNotificationTemplateResponse (Maybe Text)
pntARN = lens _pntARN (\s a -> s {_pntARN = a})

-- | The default message template that's used for push notification channels.
pntDefault :: Lens' PushNotificationTemplateResponse (Maybe DefaultPushNotificationTemplate)
pntDefault = lens _pntDefault (\s a -> s {_pntDefault = a})

-- | The custom description of the message template.
pntTemplateDescription :: Lens' PushNotificationTemplateResponse (Maybe Text)
pntTemplateDescription = lens _pntTemplateDescription (\s a -> s {_pntTemplateDescription = a})

-- | The message template that's used for the GCM channel, which is used to send notifications through the Firebase Cloud Messaging (FCM), formerly Google Cloud Messaging (GCM), service. This message template overrides the default template for push notification channels (DefaultPushNotificationTemplate).
pntGCM :: Lens' PushNotificationTemplateResponse (Maybe AndroidPushNotificationTemplate)
pntGCM = lens _pntGCM (\s a -> s {_pntGCM = a})

-- | The message template that's used for the APNs (Apple Push Notification service) channel. This message template overrides the default template for push notification channels (DefaultPushNotificationTemplate).
pntAPNS :: Lens' PushNotificationTemplateResponse (Maybe APNSPushNotificationTemplate)
pntAPNS = lens _pntAPNS (\s a -> s {_pntAPNS = a})

-- | The JSON object that specifies the default values that are used for message variables in the message template. This object is a set of key-value pairs. Each key defines a message variable in the template. The corresponding value defines the default value for that variable.
pntDefaultSubstitutions :: Lens' PushNotificationTemplateResponse (Maybe Text)
pntDefaultSubstitutions = lens _pntDefaultSubstitutions (\s a -> s {_pntDefaultSubstitutions = a})

-- | The unique identifier, as an integer, for the active version of the message template, or the version of the template that you specified by using the version parameter in your request.
pntVersion :: Lens' PushNotificationTemplateResponse (Maybe Text)
pntVersion = lens _pntVersion (\s a -> s {_pntVersion = a})

-- | The message template that's used for the ADM (Amazon Device Messaging) channel. This message template overrides the default template for push notification channels (DefaultPushNotificationTemplate).
pntADM :: Lens' PushNotificationTemplateResponse (Maybe AndroidPushNotificationTemplate)
pntADM = lens _pntADM (\s a -> s {_pntADM = a})

-- | The message template that's used for the Baidu (Baidu Cloud Push) channel. This message template overrides the default template for push notification channels (DefaultPushNotificationTemplate).
pntBaidu :: Lens' PushNotificationTemplateResponse (Maybe AndroidPushNotificationTemplate)
pntBaidu = lens _pntBaidu (\s a -> s {_pntBaidu = a})

-- | The unique identifier for the recommender model that's used by the message template.
pntRecommenderId :: Lens' PushNotificationTemplateResponse (Maybe Text)
pntRecommenderId = lens _pntRecommenderId (\s a -> s {_pntRecommenderId = a})

-- | A string-to-string map of key-value pairs that identifies the tags that are associated with the message template. Each tag consists of a required tag key and an associated tag value.
pntTags :: Lens' PushNotificationTemplateResponse (HashMap Text (Text))
pntTags = lens _pntTags (\s a -> s {_pntTags = a}) . _Default . _Map

-- | The date, in ISO 8601 format, when the message template was last modified.
pntLastModifiedDate :: Lens' PushNotificationTemplateResponse Text
pntLastModifiedDate = lens _pntLastModifiedDate (\s a -> s {_pntLastModifiedDate = a})

-- | The date, in ISO 8601 format, when the message template was created.
pntCreationDate :: Lens' PushNotificationTemplateResponse Text
pntCreationDate = lens _pntCreationDate (\s a -> s {_pntCreationDate = a})

-- | The type of channel that the message template is designed for. For a push notification template, this value is PUSH.
pntTemplateType :: Lens' PushNotificationTemplateResponse TemplateType
pntTemplateType = lens _pntTemplateType (\s a -> s {_pntTemplateType = a})

-- | The name of the message template.
pntTemplateName :: Lens' PushNotificationTemplateResponse Text
pntTemplateName = lens _pntTemplateName (\s a -> s {_pntTemplateName = a})

instance FromJSON PushNotificationTemplateResponse where
  parseJSON =
    withObject
      "PushNotificationTemplateResponse"
      ( \x ->
          PushNotificationTemplateResponse'
            <$> (x .:? "Arn")
            <*> (x .:? "Default")
            <*> (x .:? "TemplateDescription")
            <*> (x .:? "GCM")
            <*> (x .:? "APNS")
            <*> (x .:? "DefaultSubstitutions")
            <*> (x .:? "Version")
            <*> (x .:? "ADM")
            <*> (x .:? "Baidu")
            <*> (x .:? "RecommenderId")
            <*> (x .:? "tags" .!= mempty)
            <*> (x .: "LastModifiedDate")
            <*> (x .: "CreationDate")
            <*> (x .: "TemplateType")
            <*> (x .: "TemplateName")
      )

instance Hashable PushNotificationTemplateResponse

instance NFData PushNotificationTemplateResponse

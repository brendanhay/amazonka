{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.PushNotificationTemplateRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.PushNotificationTemplateRequest where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types.APNSPushNotificationTemplate
import Network.AWS.Pinpoint.Types.AndroidPushNotificationTemplate
import Network.AWS.Pinpoint.Types.DefaultPushNotificationTemplate
import Network.AWS.Prelude

-- | Specifies the content and settings for a message template that can be used in messages that are sent through a push notification channel.
--
--
--
-- /See:/ 'pushNotificationTemplateRequest' smart constructor.
data PushNotificationTemplateRequest = PushNotificationTemplateRequest'
  { _pntrDefault ::
      !( Maybe
           DefaultPushNotificationTemplate
       ),
    _pntrTemplateDescription ::
      !(Maybe Text),
    _pntrGCM ::
      !( Maybe
           AndroidPushNotificationTemplate
       ),
    _pntrAPNS ::
      !( Maybe
           APNSPushNotificationTemplate
       ),
    _pntrDefaultSubstitutions ::
      !(Maybe Text),
    _pntrADM ::
      !( Maybe
           AndroidPushNotificationTemplate
       ),
    _pntrBaidu ::
      !( Maybe
           AndroidPushNotificationTemplate
       ),
    _pntrRecommenderId ::
      !(Maybe Text),
    _pntrTags ::
      !(Maybe (Map Text (Text)))
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PushNotificationTemplateRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pntrDefault' - The default message template to use for push notification channels.
--
-- * 'pntrTemplateDescription' - A custom description of the message template.
--
-- * 'pntrGCM' - The message template to use for the GCM channel, which is used to send notifications through the Firebase Cloud Messaging (FCM), formerly Google Cloud Messaging (GCM), service. This message template overrides the default template for push notification channels (DefaultPushNotificationTemplate).
--
-- * 'pntrAPNS' - The message template to use for the APNs (Apple Push Notification service) channel. This message template overrides the default template for push notification channels (DefaultPushNotificationTemplate).
--
-- * 'pntrDefaultSubstitutions' - A JSON object that specifies the default values to use for message variables in the message template. This object is a set of key-value pairs. Each key defines a message variable in the template. The corresponding value defines the default value for that variable. When you create a message that's based on the template, you can override these defaults with message-specific and address-specific variables and values.
--
-- * 'pntrADM' - The message template to use for the ADM (Amazon Device Messaging) channel. This message template overrides the default template for push notification channels (DefaultPushNotificationTemplate).
--
-- * 'pntrBaidu' - The message template to use for the Baidu (Baidu Cloud Push) channel. This message template overrides the default template for push notification channels (DefaultPushNotificationTemplate).
--
-- * 'pntrRecommenderId' - The unique identifier for the recommender model to use for the message template. Amazon Pinpoint uses this value to determine how to retrieve and process data from a recommender model when it sends messages that use the template, if the template contains message variables for recommendation data.
--
-- * 'pntrTags' - A string-to-string map of key-value pairs that defines the tags to associate with the message template. Each tag consists of a required tag key and an associated tag value.
pushNotificationTemplateRequest ::
  PushNotificationTemplateRequest
pushNotificationTemplateRequest =
  PushNotificationTemplateRequest'
    { _pntrDefault = Nothing,
      _pntrTemplateDescription = Nothing,
      _pntrGCM = Nothing,
      _pntrAPNS = Nothing,
      _pntrDefaultSubstitutions = Nothing,
      _pntrADM = Nothing,
      _pntrBaidu = Nothing,
      _pntrRecommenderId = Nothing,
      _pntrTags = Nothing
    }

-- | The default message template to use for push notification channels.
pntrDefault :: Lens' PushNotificationTemplateRequest (Maybe DefaultPushNotificationTemplate)
pntrDefault = lens _pntrDefault (\s a -> s {_pntrDefault = a})

-- | A custom description of the message template.
pntrTemplateDescription :: Lens' PushNotificationTemplateRequest (Maybe Text)
pntrTemplateDescription = lens _pntrTemplateDescription (\s a -> s {_pntrTemplateDescription = a})

-- | The message template to use for the GCM channel, which is used to send notifications through the Firebase Cloud Messaging (FCM), formerly Google Cloud Messaging (GCM), service. This message template overrides the default template for push notification channels (DefaultPushNotificationTemplate).
pntrGCM :: Lens' PushNotificationTemplateRequest (Maybe AndroidPushNotificationTemplate)
pntrGCM = lens _pntrGCM (\s a -> s {_pntrGCM = a})

-- | The message template to use for the APNs (Apple Push Notification service) channel. This message template overrides the default template for push notification channels (DefaultPushNotificationTemplate).
pntrAPNS :: Lens' PushNotificationTemplateRequest (Maybe APNSPushNotificationTemplate)
pntrAPNS = lens _pntrAPNS (\s a -> s {_pntrAPNS = a})

-- | A JSON object that specifies the default values to use for message variables in the message template. This object is a set of key-value pairs. Each key defines a message variable in the template. The corresponding value defines the default value for that variable. When you create a message that's based on the template, you can override these defaults with message-specific and address-specific variables and values.
pntrDefaultSubstitutions :: Lens' PushNotificationTemplateRequest (Maybe Text)
pntrDefaultSubstitutions = lens _pntrDefaultSubstitutions (\s a -> s {_pntrDefaultSubstitutions = a})

-- | The message template to use for the ADM (Amazon Device Messaging) channel. This message template overrides the default template for push notification channels (DefaultPushNotificationTemplate).
pntrADM :: Lens' PushNotificationTemplateRequest (Maybe AndroidPushNotificationTemplate)
pntrADM = lens _pntrADM (\s a -> s {_pntrADM = a})

-- | The message template to use for the Baidu (Baidu Cloud Push) channel. This message template overrides the default template for push notification channels (DefaultPushNotificationTemplate).
pntrBaidu :: Lens' PushNotificationTemplateRequest (Maybe AndroidPushNotificationTemplate)
pntrBaidu = lens _pntrBaidu (\s a -> s {_pntrBaidu = a})

-- | The unique identifier for the recommender model to use for the message template. Amazon Pinpoint uses this value to determine how to retrieve and process data from a recommender model when it sends messages that use the template, if the template contains message variables for recommendation data.
pntrRecommenderId :: Lens' PushNotificationTemplateRequest (Maybe Text)
pntrRecommenderId = lens _pntrRecommenderId (\s a -> s {_pntrRecommenderId = a})

-- | A string-to-string map of key-value pairs that defines the tags to associate with the message template. Each tag consists of a required tag key and an associated tag value.
pntrTags :: Lens' PushNotificationTemplateRequest (HashMap Text (Text))
pntrTags = lens _pntrTags (\s a -> s {_pntrTags = a}) . _Default . _Map

instance Hashable PushNotificationTemplateRequest

instance NFData PushNotificationTemplateRequest

instance ToJSON PushNotificationTemplateRequest where
  toJSON PushNotificationTemplateRequest' {..} =
    object
      ( catMaybes
          [ ("Default" .=) <$> _pntrDefault,
            ("TemplateDescription" .=) <$> _pntrTemplateDescription,
            ("GCM" .=) <$> _pntrGCM,
            ("APNS" .=) <$> _pntrAPNS,
            ("DefaultSubstitutions" .=) <$> _pntrDefaultSubstitutions,
            ("ADM" .=) <$> _pntrADM,
            ("Baidu" .=) <$> _pntrBaidu,
            ("RecommenderId" .=) <$> _pntrRecommenderId,
            ("tags" .=) <$> _pntrTags
          ]
      )

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.MessageConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.MessageConfiguration where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types.CampaignCustomMessage
import Network.AWS.Pinpoint.Types.CampaignEmailMessage
import Network.AWS.Pinpoint.Types.CampaignSmsMessage
import Network.AWS.Pinpoint.Types.Message
import Network.AWS.Prelude

-- | Specifies the message configuration settings for a campaign.
--
--
--
-- /See:/ 'messageConfiguration' smart constructor.
data MessageConfiguration = MessageConfiguration'
  { _mcAPNSMessage ::
      !(Maybe Message),
    _mcGCMMessage :: !(Maybe Message),
    _mcDefaultMessage :: !(Maybe Message),
    _mcCustomMessage ::
      !(Maybe CampaignCustomMessage),
    _mcADMMessage :: !(Maybe Message),
    _mcSMSMessage :: !(Maybe CampaignSmsMessage),
    _mcEmailMessage :: !(Maybe CampaignEmailMessage),
    _mcBaiduMessage :: !(Maybe Message)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MessageConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mcAPNSMessage' - The message that the campaign sends through the APNs (Apple Push Notification service) channel. If specified, this message overrides the default message.
--
-- * 'mcGCMMessage' - The message that the campaign sends through the GCM channel, which enables Amazon Pinpoint to send push notifications through the Firebase Cloud Messaging (FCM), formerly Google Cloud Messaging (GCM), service. If specified, this message overrides the default message.
--
-- * 'mcDefaultMessage' - The default message that the campaign sends through all the channels that are configured for the campaign.
--
-- * 'mcCustomMessage' - The message that the campaign sends through a custom channel, as specified by the delivery configuration (CustomDeliveryConfiguration) settings for the campaign. If specified, this message overrides the default message.
--
-- * 'mcADMMessage' - The message that the campaign sends through the ADM (Amazon Device Messaging) channel. If specified, this message overrides the default message.
--
-- * 'mcSMSMessage' - The message that the campaign sends through the SMS channel. If specified, this message overrides the default message.
--
-- * 'mcEmailMessage' - The message that the campaign sends through the email channel. If specified, this message overrides the default message.
--
-- * 'mcBaiduMessage' - The message that the campaign sends through the Baidu (Baidu Cloud Push) channel. If specified, this message overrides the default message.
messageConfiguration ::
  MessageConfiguration
messageConfiguration =
  MessageConfiguration'
    { _mcAPNSMessage = Nothing,
      _mcGCMMessage = Nothing,
      _mcDefaultMessage = Nothing,
      _mcCustomMessage = Nothing,
      _mcADMMessage = Nothing,
      _mcSMSMessage = Nothing,
      _mcEmailMessage = Nothing,
      _mcBaiduMessage = Nothing
    }

-- | The message that the campaign sends through the APNs (Apple Push Notification service) channel. If specified, this message overrides the default message.
mcAPNSMessage :: Lens' MessageConfiguration (Maybe Message)
mcAPNSMessage = lens _mcAPNSMessage (\s a -> s {_mcAPNSMessage = a})

-- | The message that the campaign sends through the GCM channel, which enables Amazon Pinpoint to send push notifications through the Firebase Cloud Messaging (FCM), formerly Google Cloud Messaging (GCM), service. If specified, this message overrides the default message.
mcGCMMessage :: Lens' MessageConfiguration (Maybe Message)
mcGCMMessage = lens _mcGCMMessage (\s a -> s {_mcGCMMessage = a})

-- | The default message that the campaign sends through all the channels that are configured for the campaign.
mcDefaultMessage :: Lens' MessageConfiguration (Maybe Message)
mcDefaultMessage = lens _mcDefaultMessage (\s a -> s {_mcDefaultMessage = a})

-- | The message that the campaign sends through a custom channel, as specified by the delivery configuration (CustomDeliveryConfiguration) settings for the campaign. If specified, this message overrides the default message.
mcCustomMessage :: Lens' MessageConfiguration (Maybe CampaignCustomMessage)
mcCustomMessage = lens _mcCustomMessage (\s a -> s {_mcCustomMessage = a})

-- | The message that the campaign sends through the ADM (Amazon Device Messaging) channel. If specified, this message overrides the default message.
mcADMMessage :: Lens' MessageConfiguration (Maybe Message)
mcADMMessage = lens _mcADMMessage (\s a -> s {_mcADMMessage = a})

-- | The message that the campaign sends through the SMS channel. If specified, this message overrides the default message.
mcSMSMessage :: Lens' MessageConfiguration (Maybe CampaignSmsMessage)
mcSMSMessage = lens _mcSMSMessage (\s a -> s {_mcSMSMessage = a})

-- | The message that the campaign sends through the email channel. If specified, this message overrides the default message.
mcEmailMessage :: Lens' MessageConfiguration (Maybe CampaignEmailMessage)
mcEmailMessage = lens _mcEmailMessage (\s a -> s {_mcEmailMessage = a})

-- | The message that the campaign sends through the Baidu (Baidu Cloud Push) channel. If specified, this message overrides the default message.
mcBaiduMessage :: Lens' MessageConfiguration (Maybe Message)
mcBaiduMessage = lens _mcBaiduMessage (\s a -> s {_mcBaiduMessage = a})

instance FromJSON MessageConfiguration where
  parseJSON =
    withObject
      "MessageConfiguration"
      ( \x ->
          MessageConfiguration'
            <$> (x .:? "APNSMessage")
            <*> (x .:? "GCMMessage")
            <*> (x .:? "DefaultMessage")
            <*> (x .:? "CustomMessage")
            <*> (x .:? "ADMMessage")
            <*> (x .:? "SMSMessage")
            <*> (x .:? "EmailMessage")
            <*> (x .:? "BaiduMessage")
      )

instance Hashable MessageConfiguration

instance NFData MessageConfiguration

instance ToJSON MessageConfiguration where
  toJSON MessageConfiguration' {..} =
    object
      ( catMaybes
          [ ("APNSMessage" .=) <$> _mcAPNSMessage,
            ("GCMMessage" .=) <$> _mcGCMMessage,
            ("DefaultMessage" .=) <$> _mcDefaultMessage,
            ("CustomMessage" .=) <$> _mcCustomMessage,
            ("ADMMessage" .=) <$> _mcADMMessage,
            ("SMSMessage" .=) <$> _mcSMSMessage,
            ("EmailMessage" .=) <$> _mcEmailMessage,
            ("BaiduMessage" .=) <$> _mcBaiduMessage
          ]
      )

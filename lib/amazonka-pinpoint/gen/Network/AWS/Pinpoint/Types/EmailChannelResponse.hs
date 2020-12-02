{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.EmailChannelResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.EmailChannelResponse where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides information about the status and settings of the email channel for an application.
--
--
--
-- /See:/ 'emailChannelResponse' smart constructor.
data EmailChannelResponse = EmailChannelResponse'
  { _ecMessagesPerSecond ::
      !(Maybe Int),
    _ecLastModifiedDate :: !(Maybe Text),
    _ecEnabled :: !(Maybe Bool),
    _ecFromAddress :: !(Maybe Text),
    _ecIsArchived :: !(Maybe Bool),
    _ecApplicationId :: !(Maybe Text),
    _ecVersion :: !(Maybe Int),
    _ecConfigurationSet :: !(Maybe Text),
    _ecId :: !(Maybe Text),
    _ecCreationDate :: !(Maybe Text),
    _ecLastModifiedBy :: !(Maybe Text),
    _ecIdentity :: !(Maybe Text),
    _ecHasCredential :: !(Maybe Bool),
    _ecRoleARN :: !(Maybe Text),
    _ecPlatform :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EmailChannelResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ecMessagesPerSecond' - The maximum number of emails that can be sent through the channel each second.
--
-- * 'ecLastModifiedDate' - The date and time, in ISO 8601 format, when the email channel was last modified.
--
-- * 'ecEnabled' - Specifies whether the email channel is enabled for the application.
--
-- * 'ecFromAddress' - The verified email address that email is sent from when you send email through the channel.
--
-- * 'ecIsArchived' - Specifies whether the email channel is archived.
--
-- * 'ecApplicationId' - The unique identifier for the application that the email channel applies to.
--
-- * 'ecVersion' - The current version of the email channel.
--
-- * 'ecConfigurationSet' - The <https://docs.aws.amazon.com/ses/latest/APIReference/API_ConfigurationSet.html Amazon SES configuration set> that's applied to messages that are sent through the channel.
--
-- * 'ecId' - (Deprecated) An identifier for the email channel. This property is retained only for backward compatibility.
--
-- * 'ecCreationDate' - The date and time, in ISO 8601 format, when the email channel was enabled.
--
-- * 'ecLastModifiedBy' - The user who last modified the email channel.
--
-- * 'ecIdentity' - The Amazon Resource Name (ARN) of the identity, verified with Amazon Simple Email Service (Amazon SES), that's used when you send email through the channel.
--
-- * 'ecHasCredential' - (Not used) This property is retained only for backward compatibility.
--
-- * 'ecRoleARN' - The ARN of the AWS Identity and Access Management (IAM) role that Amazon Pinpoint uses to submit email-related event data for the channel.
--
-- * 'ecPlatform' - The type of messaging or notification platform for the channel. For the email channel, this value is EMAIL.
emailChannelResponse ::
  -- | 'ecPlatform'
  Text ->
  EmailChannelResponse
emailChannelResponse pPlatform_ =
  EmailChannelResponse'
    { _ecMessagesPerSecond = Nothing,
      _ecLastModifiedDate = Nothing,
      _ecEnabled = Nothing,
      _ecFromAddress = Nothing,
      _ecIsArchived = Nothing,
      _ecApplicationId = Nothing,
      _ecVersion = Nothing,
      _ecConfigurationSet = Nothing,
      _ecId = Nothing,
      _ecCreationDate = Nothing,
      _ecLastModifiedBy = Nothing,
      _ecIdentity = Nothing,
      _ecHasCredential = Nothing,
      _ecRoleARN = Nothing,
      _ecPlatform = pPlatform_
    }

-- | The maximum number of emails that can be sent through the channel each second.
ecMessagesPerSecond :: Lens' EmailChannelResponse (Maybe Int)
ecMessagesPerSecond = lens _ecMessagesPerSecond (\s a -> s {_ecMessagesPerSecond = a})

-- | The date and time, in ISO 8601 format, when the email channel was last modified.
ecLastModifiedDate :: Lens' EmailChannelResponse (Maybe Text)
ecLastModifiedDate = lens _ecLastModifiedDate (\s a -> s {_ecLastModifiedDate = a})

-- | Specifies whether the email channel is enabled for the application.
ecEnabled :: Lens' EmailChannelResponse (Maybe Bool)
ecEnabled = lens _ecEnabled (\s a -> s {_ecEnabled = a})

-- | The verified email address that email is sent from when you send email through the channel.
ecFromAddress :: Lens' EmailChannelResponse (Maybe Text)
ecFromAddress = lens _ecFromAddress (\s a -> s {_ecFromAddress = a})

-- | Specifies whether the email channel is archived.
ecIsArchived :: Lens' EmailChannelResponse (Maybe Bool)
ecIsArchived = lens _ecIsArchived (\s a -> s {_ecIsArchived = a})

-- | The unique identifier for the application that the email channel applies to.
ecApplicationId :: Lens' EmailChannelResponse (Maybe Text)
ecApplicationId = lens _ecApplicationId (\s a -> s {_ecApplicationId = a})

-- | The current version of the email channel.
ecVersion :: Lens' EmailChannelResponse (Maybe Int)
ecVersion = lens _ecVersion (\s a -> s {_ecVersion = a})

-- | The <https://docs.aws.amazon.com/ses/latest/APIReference/API_ConfigurationSet.html Amazon SES configuration set> that's applied to messages that are sent through the channel.
ecConfigurationSet :: Lens' EmailChannelResponse (Maybe Text)
ecConfigurationSet = lens _ecConfigurationSet (\s a -> s {_ecConfigurationSet = a})

-- | (Deprecated) An identifier for the email channel. This property is retained only for backward compatibility.
ecId :: Lens' EmailChannelResponse (Maybe Text)
ecId = lens _ecId (\s a -> s {_ecId = a})

-- | The date and time, in ISO 8601 format, when the email channel was enabled.
ecCreationDate :: Lens' EmailChannelResponse (Maybe Text)
ecCreationDate = lens _ecCreationDate (\s a -> s {_ecCreationDate = a})

-- | The user who last modified the email channel.
ecLastModifiedBy :: Lens' EmailChannelResponse (Maybe Text)
ecLastModifiedBy = lens _ecLastModifiedBy (\s a -> s {_ecLastModifiedBy = a})

-- | The Amazon Resource Name (ARN) of the identity, verified with Amazon Simple Email Service (Amazon SES), that's used when you send email through the channel.
ecIdentity :: Lens' EmailChannelResponse (Maybe Text)
ecIdentity = lens _ecIdentity (\s a -> s {_ecIdentity = a})

-- | (Not used) This property is retained only for backward compatibility.
ecHasCredential :: Lens' EmailChannelResponse (Maybe Bool)
ecHasCredential = lens _ecHasCredential (\s a -> s {_ecHasCredential = a})

-- | The ARN of the AWS Identity and Access Management (IAM) role that Amazon Pinpoint uses to submit email-related event data for the channel.
ecRoleARN :: Lens' EmailChannelResponse (Maybe Text)
ecRoleARN = lens _ecRoleARN (\s a -> s {_ecRoleARN = a})

-- | The type of messaging or notification platform for the channel. For the email channel, this value is EMAIL.
ecPlatform :: Lens' EmailChannelResponse Text
ecPlatform = lens _ecPlatform (\s a -> s {_ecPlatform = a})

instance FromJSON EmailChannelResponse where
  parseJSON =
    withObject
      "EmailChannelResponse"
      ( \x ->
          EmailChannelResponse'
            <$> (x .:? "MessagesPerSecond")
            <*> (x .:? "LastModifiedDate")
            <*> (x .:? "Enabled")
            <*> (x .:? "FromAddress")
            <*> (x .:? "IsArchived")
            <*> (x .:? "ApplicationId")
            <*> (x .:? "Version")
            <*> (x .:? "ConfigurationSet")
            <*> (x .:? "Id")
            <*> (x .:? "CreationDate")
            <*> (x .:? "LastModifiedBy")
            <*> (x .:? "Identity")
            <*> (x .:? "HasCredential")
            <*> (x .:? "RoleArn")
            <*> (x .: "Platform")
      )

instance Hashable EmailChannelResponse

instance NFData EmailChannelResponse

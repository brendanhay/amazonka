{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.EmailChannelRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.EmailChannelRequest where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies the status and settings of the email channel for an application.
--
--
--
-- /See:/ 'emailChannelRequest' smart constructor.
data EmailChannelRequest = EmailChannelRequest'
  { _ecrEnabled ::
      !(Maybe Bool),
    _ecrConfigurationSet :: !(Maybe Text),
    _ecrRoleARN :: !(Maybe Text),
    _ecrFromAddress :: !Text,
    _ecrIdentity :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EmailChannelRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ecrEnabled' - Specifies whether to enable the email channel for the application.
--
-- * 'ecrConfigurationSet' - The <https://docs.aws.amazon.com/ses/latest/APIReference/API_ConfigurationSet.html Amazon SES configuration set> that you want to apply to messages that you send through the channel.
--
-- * 'ecrRoleARN' - The ARN of the AWS Identity and Access Management (IAM) role that you want Amazon Pinpoint to use when it submits email-related event data for the channel.
--
-- * 'ecrFromAddress' - The verified email address that you want to send email from when you send email through the channel.
--
-- * 'ecrIdentity' - The Amazon Resource Name (ARN) of the identity, verified with Amazon Simple Email Service (Amazon SES), that you want to use when you send email through the channel.
emailChannelRequest ::
  -- | 'ecrFromAddress'
  Text ->
  -- | 'ecrIdentity'
  Text ->
  EmailChannelRequest
emailChannelRequest pFromAddress_ pIdentity_ =
  EmailChannelRequest'
    { _ecrEnabled = Nothing,
      _ecrConfigurationSet = Nothing,
      _ecrRoleARN = Nothing,
      _ecrFromAddress = pFromAddress_,
      _ecrIdentity = pIdentity_
    }

-- | Specifies whether to enable the email channel for the application.
ecrEnabled :: Lens' EmailChannelRequest (Maybe Bool)
ecrEnabled = lens _ecrEnabled (\s a -> s {_ecrEnabled = a})

-- | The <https://docs.aws.amazon.com/ses/latest/APIReference/API_ConfigurationSet.html Amazon SES configuration set> that you want to apply to messages that you send through the channel.
ecrConfigurationSet :: Lens' EmailChannelRequest (Maybe Text)
ecrConfigurationSet = lens _ecrConfigurationSet (\s a -> s {_ecrConfigurationSet = a})

-- | The ARN of the AWS Identity and Access Management (IAM) role that you want Amazon Pinpoint to use when it submits email-related event data for the channel.
ecrRoleARN :: Lens' EmailChannelRequest (Maybe Text)
ecrRoleARN = lens _ecrRoleARN (\s a -> s {_ecrRoleARN = a})

-- | The verified email address that you want to send email from when you send email through the channel.
ecrFromAddress :: Lens' EmailChannelRequest Text
ecrFromAddress = lens _ecrFromAddress (\s a -> s {_ecrFromAddress = a})

-- | The Amazon Resource Name (ARN) of the identity, verified with Amazon Simple Email Service (Amazon SES), that you want to use when you send email through the channel.
ecrIdentity :: Lens' EmailChannelRequest Text
ecrIdentity = lens _ecrIdentity (\s a -> s {_ecrIdentity = a})

instance Hashable EmailChannelRequest

instance NFData EmailChannelRequest

instance ToJSON EmailChannelRequest where
  toJSON EmailChannelRequest' {..} =
    object
      ( catMaybes
          [ ("Enabled" .=) <$> _ecrEnabled,
            ("ConfigurationSet" .=) <$> _ecrConfigurationSet,
            ("RoleArn" .=) <$> _ecrRoleARN,
            Just ("FromAddress" .= _ecrFromAddress),
            Just ("Identity" .= _ecrIdentity)
          ]
      )

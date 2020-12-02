{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.WebhookAuthConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.WebhookAuthConfiguration where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The authentication applied to incoming webhook trigger requests.
--
--
--
-- /See:/ 'webhookAuthConfiguration' smart constructor.
data WebhookAuthConfiguration = WebhookAuthConfiguration'
  { _wacAllowedIPRange ::
      !(Maybe Text),
    _wacSecretToken :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'WebhookAuthConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'wacAllowedIPRange' - The property used to configure acceptance of webhooks in an IP address range. For IP, only the @AllowedIPRange@ property must be set. This property must be set to a valid CIDR range.
--
-- * 'wacSecretToken' - The property used to configure GitHub authentication. For GITHUB_HMAC, only the @SecretToken@ property must be set.
webhookAuthConfiguration ::
  WebhookAuthConfiguration
webhookAuthConfiguration =
  WebhookAuthConfiguration'
    { _wacAllowedIPRange = Nothing,
      _wacSecretToken = Nothing
    }

-- | The property used to configure acceptance of webhooks in an IP address range. For IP, only the @AllowedIPRange@ property must be set. This property must be set to a valid CIDR range.
wacAllowedIPRange :: Lens' WebhookAuthConfiguration (Maybe Text)
wacAllowedIPRange = lens _wacAllowedIPRange (\s a -> s {_wacAllowedIPRange = a})

-- | The property used to configure GitHub authentication. For GITHUB_HMAC, only the @SecretToken@ property must be set.
wacSecretToken :: Lens' WebhookAuthConfiguration (Maybe Text)
wacSecretToken = lens _wacSecretToken (\s a -> s {_wacSecretToken = a})

instance FromJSON WebhookAuthConfiguration where
  parseJSON =
    withObject
      "WebhookAuthConfiguration"
      ( \x ->
          WebhookAuthConfiguration'
            <$> (x .:? "AllowedIPRange") <*> (x .:? "SecretToken")
      )

instance Hashable WebhookAuthConfiguration

instance NFData WebhookAuthConfiguration

instance ToJSON WebhookAuthConfiguration where
  toJSON WebhookAuthConfiguration' {..} =
    object
      ( catMaybes
          [ ("AllowedIPRange" .=) <$> _wacAllowedIPRange,
            ("SecretToken" .=) <$> _wacSecretToken
          ]
      )

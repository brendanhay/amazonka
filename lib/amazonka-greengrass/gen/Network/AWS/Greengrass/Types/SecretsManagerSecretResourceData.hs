{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.SecretsManagerSecretResourceData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.SecretsManagerSecretResourceData where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Attributes that define a secret resource, which references a secret from AWS Secrets Manager. AWS IoT Greengrass stores a local, encrypted copy of the secret on the Greengrass core, where it can be securely accessed by connectors and Lambda functions.
--
-- /See:/ 'secretsManagerSecretResourceData' smart constructor.
data SecretsManagerSecretResourceData = SecretsManagerSecretResourceData'
  { _smsrdAdditionalStagingLabelsToDownload ::
      !(Maybe [Text]),
    _smsrdARN ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SecretsManagerSecretResourceData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'smsrdAdditionalStagingLabelsToDownload' - Optional. The staging labels whose values you want to make available on the core, in addition to ''AWSCURRENT''.
--
-- * 'smsrdARN' - The ARN of the Secrets Manager secret to make available on the core. The value of the secret's latest version (represented by the ''AWSCURRENT'' staging label) is included by default.
secretsManagerSecretResourceData ::
  SecretsManagerSecretResourceData
secretsManagerSecretResourceData =
  SecretsManagerSecretResourceData'
    { _smsrdAdditionalStagingLabelsToDownload =
        Nothing,
      _smsrdARN = Nothing
    }

-- | Optional. The staging labels whose values you want to make available on the core, in addition to ''AWSCURRENT''.
smsrdAdditionalStagingLabelsToDownload :: Lens' SecretsManagerSecretResourceData [Text]
smsrdAdditionalStagingLabelsToDownload = lens _smsrdAdditionalStagingLabelsToDownload (\s a -> s {_smsrdAdditionalStagingLabelsToDownload = a}) . _Default . _Coerce

-- | The ARN of the Secrets Manager secret to make available on the core. The value of the secret's latest version (represented by the ''AWSCURRENT'' staging label) is included by default.
smsrdARN :: Lens' SecretsManagerSecretResourceData (Maybe Text)
smsrdARN = lens _smsrdARN (\s a -> s {_smsrdARN = a})

instance FromJSON SecretsManagerSecretResourceData where
  parseJSON =
    withObject
      "SecretsManagerSecretResourceData"
      ( \x ->
          SecretsManagerSecretResourceData'
            <$> (x .:? "AdditionalStagingLabelsToDownload" .!= mempty)
            <*> (x .:? "ARN")
      )

instance Hashable SecretsManagerSecretResourceData

instance NFData SecretsManagerSecretResourceData

instance ToJSON SecretsManagerSecretResourceData where
  toJSON SecretsManagerSecretResourceData' {..} =
    object
      ( catMaybes
          [ ("AdditionalStagingLabelsToDownload" .=)
              <$> _smsrdAdditionalStagingLabelsToDownload,
            ("ARN" .=) <$> _smsrdARN
          ]
      )

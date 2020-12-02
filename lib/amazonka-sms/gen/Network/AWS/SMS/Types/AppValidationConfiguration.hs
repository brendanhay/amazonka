{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.AppValidationConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.AppValidationConfiguration where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SMS.Types.AppValidationStrategy
import Network.AWS.SMS.Types.SSMValidationParameters

-- | Configuration for validating an application.
--
--
--
-- /See:/ 'appValidationConfiguration' smart constructor.
data AppValidationConfiguration = AppValidationConfiguration'
  { _avcSsmValidationParameters ::
      !(Maybe SSMValidationParameters),
    _avcName :: !(Maybe Text),
    _avcValidationId :: !(Maybe Text),
    _avcAppValidationStrategy ::
      !(Maybe AppValidationStrategy)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AppValidationConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'avcSsmValidationParameters' - The validation parameters.
--
-- * 'avcName' - The name of the configuration.
--
-- * 'avcValidationId' - The ID of the validation.
--
-- * 'avcAppValidationStrategy' - The validation strategy.
appValidationConfiguration ::
  AppValidationConfiguration
appValidationConfiguration =
  AppValidationConfiguration'
    { _avcSsmValidationParameters =
        Nothing,
      _avcName = Nothing,
      _avcValidationId = Nothing,
      _avcAppValidationStrategy = Nothing
    }

-- | The validation parameters.
avcSsmValidationParameters :: Lens' AppValidationConfiguration (Maybe SSMValidationParameters)
avcSsmValidationParameters = lens _avcSsmValidationParameters (\s a -> s {_avcSsmValidationParameters = a})

-- | The name of the configuration.
avcName :: Lens' AppValidationConfiguration (Maybe Text)
avcName = lens _avcName (\s a -> s {_avcName = a})

-- | The ID of the validation.
avcValidationId :: Lens' AppValidationConfiguration (Maybe Text)
avcValidationId = lens _avcValidationId (\s a -> s {_avcValidationId = a})

-- | The validation strategy.
avcAppValidationStrategy :: Lens' AppValidationConfiguration (Maybe AppValidationStrategy)
avcAppValidationStrategy = lens _avcAppValidationStrategy (\s a -> s {_avcAppValidationStrategy = a})

instance FromJSON AppValidationConfiguration where
  parseJSON =
    withObject
      "AppValidationConfiguration"
      ( \x ->
          AppValidationConfiguration'
            <$> (x .:? "ssmValidationParameters")
            <*> (x .:? "name")
            <*> (x .:? "validationId")
            <*> (x .:? "appValidationStrategy")
      )

instance Hashable AppValidationConfiguration

instance NFData AppValidationConfiguration

instance ToJSON AppValidationConfiguration where
  toJSON AppValidationConfiguration' {..} =
    object
      ( catMaybes
          [ ("ssmValidationParameters" .=) <$> _avcSsmValidationParameters,
            ("name" .=) <$> _avcName,
            ("validationId" .=) <$> _avcValidationId,
            ("appValidationStrategy" .=) <$> _avcAppValidationStrategy
          ]
      )

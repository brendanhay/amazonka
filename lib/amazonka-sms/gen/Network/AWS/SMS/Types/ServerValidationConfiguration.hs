{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.ServerValidationConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.ServerValidationConfiguration where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SMS.Types.Server
import Network.AWS.SMS.Types.ServerValidationStrategy
import Network.AWS.SMS.Types.UserDataValidationParameters

-- | Configuration for validating an instance.
--
--
--
-- /See:/ 'serverValidationConfiguration' smart constructor.
data ServerValidationConfiguration = ServerValidationConfiguration'
  { _svcServerValidationStrategy ::
      !( Maybe
           ServerValidationStrategy
       ),
    _svcUserDataValidationParameters ::
      !( Maybe
           UserDataValidationParameters
       ),
    _svcName :: !(Maybe Text),
    _svcServer :: !(Maybe Server),
    _svcValidationId ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ServerValidationConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'svcServerValidationStrategy' - The validation strategy.
--
-- * 'svcUserDataValidationParameters' - The validation parameters.
--
-- * 'svcName' - The name of the configuration.
--
-- * 'svcServer' - Undocumented member.
--
-- * 'svcValidationId' - The ID of the validation.
serverValidationConfiguration ::
  ServerValidationConfiguration
serverValidationConfiguration =
  ServerValidationConfiguration'
    { _svcServerValidationStrategy =
        Nothing,
      _svcUserDataValidationParameters = Nothing,
      _svcName = Nothing,
      _svcServer = Nothing,
      _svcValidationId = Nothing
    }

-- | The validation strategy.
svcServerValidationStrategy :: Lens' ServerValidationConfiguration (Maybe ServerValidationStrategy)
svcServerValidationStrategy = lens _svcServerValidationStrategy (\s a -> s {_svcServerValidationStrategy = a})

-- | The validation parameters.
svcUserDataValidationParameters :: Lens' ServerValidationConfiguration (Maybe UserDataValidationParameters)
svcUserDataValidationParameters = lens _svcUserDataValidationParameters (\s a -> s {_svcUserDataValidationParameters = a})

-- | The name of the configuration.
svcName :: Lens' ServerValidationConfiguration (Maybe Text)
svcName = lens _svcName (\s a -> s {_svcName = a})

-- | Undocumented member.
svcServer :: Lens' ServerValidationConfiguration (Maybe Server)
svcServer = lens _svcServer (\s a -> s {_svcServer = a})

-- | The ID of the validation.
svcValidationId :: Lens' ServerValidationConfiguration (Maybe Text)
svcValidationId = lens _svcValidationId (\s a -> s {_svcValidationId = a})

instance FromJSON ServerValidationConfiguration where
  parseJSON =
    withObject
      "ServerValidationConfiguration"
      ( \x ->
          ServerValidationConfiguration'
            <$> (x .:? "serverValidationStrategy")
            <*> (x .:? "userDataValidationParameters")
            <*> (x .:? "name")
            <*> (x .:? "server")
            <*> (x .:? "validationId")
      )

instance Hashable ServerValidationConfiguration

instance NFData ServerValidationConfiguration

instance ToJSON ServerValidationConfiguration where
  toJSON ServerValidationConfiguration' {..} =
    object
      ( catMaybes
          [ ("serverValidationStrategy" .=) <$> _svcServerValidationStrategy,
            ("userDataValidationParameters" .=)
              <$> _svcUserDataValidationParameters,
            ("name" .=) <$> _svcName,
            ("server" .=) <$> _svcServer,
            ("validationId" .=) <$> _svcValidationId
          ]
      )

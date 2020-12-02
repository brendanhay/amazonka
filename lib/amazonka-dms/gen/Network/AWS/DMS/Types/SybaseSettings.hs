{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.SybaseSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.SybaseSettings where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides information that defines a SAP ASE endpoint.
--
--
--
-- /See:/ 'sybaseSettings' smart constructor.
data SybaseSettings = SybaseSettings'
  { _ssServerName ::
      !(Maybe Text),
    _ssUsername :: !(Maybe Text),
    _ssPassword :: !(Maybe (Sensitive Text)),
    _ssDatabaseName :: !(Maybe Text),
    _ssPort :: !(Maybe Int)
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'SybaseSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssServerName' - Fully qualified domain name of the endpoint.
--
-- * 'ssUsername' - Endpoint connection user name.
--
-- * 'ssPassword' - Endpoint connection password.
--
-- * 'ssDatabaseName' - Database name for the endpoint.
--
-- * 'ssPort' - Endpoint TCP port.
sybaseSettings ::
  SybaseSettings
sybaseSettings =
  SybaseSettings'
    { _ssServerName = Nothing,
      _ssUsername = Nothing,
      _ssPassword = Nothing,
      _ssDatabaseName = Nothing,
      _ssPort = Nothing
    }

-- | Fully qualified domain name of the endpoint.
ssServerName :: Lens' SybaseSettings (Maybe Text)
ssServerName = lens _ssServerName (\s a -> s {_ssServerName = a})

-- | Endpoint connection user name.
ssUsername :: Lens' SybaseSettings (Maybe Text)
ssUsername = lens _ssUsername (\s a -> s {_ssUsername = a})

-- | Endpoint connection password.
ssPassword :: Lens' SybaseSettings (Maybe Text)
ssPassword = lens _ssPassword (\s a -> s {_ssPassword = a}) . mapping _Sensitive

-- | Database name for the endpoint.
ssDatabaseName :: Lens' SybaseSettings (Maybe Text)
ssDatabaseName = lens _ssDatabaseName (\s a -> s {_ssDatabaseName = a})

-- | Endpoint TCP port.
ssPort :: Lens' SybaseSettings (Maybe Int)
ssPort = lens _ssPort (\s a -> s {_ssPort = a})

instance FromJSON SybaseSettings where
  parseJSON =
    withObject
      "SybaseSettings"
      ( \x ->
          SybaseSettings'
            <$> (x .:? "ServerName")
            <*> (x .:? "Username")
            <*> (x .:? "Password")
            <*> (x .:? "DatabaseName")
            <*> (x .:? "Port")
      )

instance Hashable SybaseSettings

instance NFData SybaseSettings

instance ToJSON SybaseSettings where
  toJSON SybaseSettings' {..} =
    object
      ( catMaybes
          [ ("ServerName" .=) <$> _ssServerName,
            ("Username" .=) <$> _ssUsername,
            ("Password" .=) <$> _ssPassword,
            ("DatabaseName" .=) <$> _ssDatabaseName,
            ("Port" .=) <$> _ssPort
          ]
      )

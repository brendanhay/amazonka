{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.IBMDB2Settings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.IBMDB2Settings where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides information that defines an IBM Db2 LUW endpoint.
--
--
--
-- /See:/ 'iBMDB2Settings' smart constructor.
data IBMDB2Settings = IBMDB2Settings'
  { _ibmdsServerName ::
      !(Maybe Text),
    _ibmdsCurrentLsn :: !(Maybe Text),
    _ibmdsSetDataCaptureChanges :: !(Maybe Bool),
    _ibmdsUsername :: !(Maybe Text),
    _ibmdsPassword :: !(Maybe (Sensitive Text)),
    _ibmdsDatabaseName :: !(Maybe Text),
    _ibmdsMaxKBytesPerRead :: !(Maybe Int),
    _ibmdsPort :: !(Maybe Int)
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'IBMDB2Settings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ibmdsServerName' - Fully qualified domain name of the endpoint.
--
-- * 'ibmdsCurrentLsn' - For ongoing replication (CDC), use CurrentLSN to specify a log sequence number (LSN) where you want the replication to start.
--
-- * 'ibmdsSetDataCaptureChanges' - Enables ongoing replication (CDC) as a BOOLEAN value. The default is true.
--
-- * 'ibmdsUsername' - Endpoint connection user name.
--
-- * 'ibmdsPassword' - Endpoint connection password.
--
-- * 'ibmdsDatabaseName' - Database name for the endpoint.
--
-- * 'ibmdsMaxKBytesPerRead' - Maximum number of bytes per read, as a NUMBER value. The default is 64 KB.
--
-- * 'ibmdsPort' - Endpoint TCP port.
iBMDB2Settings ::
  IBMDB2Settings
iBMDB2Settings =
  IBMDB2Settings'
    { _ibmdsServerName = Nothing,
      _ibmdsCurrentLsn = Nothing,
      _ibmdsSetDataCaptureChanges = Nothing,
      _ibmdsUsername = Nothing,
      _ibmdsPassword = Nothing,
      _ibmdsDatabaseName = Nothing,
      _ibmdsMaxKBytesPerRead = Nothing,
      _ibmdsPort = Nothing
    }

-- | Fully qualified domain name of the endpoint.
ibmdsServerName :: Lens' IBMDB2Settings (Maybe Text)
ibmdsServerName = lens _ibmdsServerName (\s a -> s {_ibmdsServerName = a})

-- | For ongoing replication (CDC), use CurrentLSN to specify a log sequence number (LSN) where you want the replication to start.
ibmdsCurrentLsn :: Lens' IBMDB2Settings (Maybe Text)
ibmdsCurrentLsn = lens _ibmdsCurrentLsn (\s a -> s {_ibmdsCurrentLsn = a})

-- | Enables ongoing replication (CDC) as a BOOLEAN value. The default is true.
ibmdsSetDataCaptureChanges :: Lens' IBMDB2Settings (Maybe Bool)
ibmdsSetDataCaptureChanges = lens _ibmdsSetDataCaptureChanges (\s a -> s {_ibmdsSetDataCaptureChanges = a})

-- | Endpoint connection user name.
ibmdsUsername :: Lens' IBMDB2Settings (Maybe Text)
ibmdsUsername = lens _ibmdsUsername (\s a -> s {_ibmdsUsername = a})

-- | Endpoint connection password.
ibmdsPassword :: Lens' IBMDB2Settings (Maybe Text)
ibmdsPassword = lens _ibmdsPassword (\s a -> s {_ibmdsPassword = a}) . mapping _Sensitive

-- | Database name for the endpoint.
ibmdsDatabaseName :: Lens' IBMDB2Settings (Maybe Text)
ibmdsDatabaseName = lens _ibmdsDatabaseName (\s a -> s {_ibmdsDatabaseName = a})

-- | Maximum number of bytes per read, as a NUMBER value. The default is 64 KB.
ibmdsMaxKBytesPerRead :: Lens' IBMDB2Settings (Maybe Int)
ibmdsMaxKBytesPerRead = lens _ibmdsMaxKBytesPerRead (\s a -> s {_ibmdsMaxKBytesPerRead = a})

-- | Endpoint TCP port.
ibmdsPort :: Lens' IBMDB2Settings (Maybe Int)
ibmdsPort = lens _ibmdsPort (\s a -> s {_ibmdsPort = a})

instance FromJSON IBMDB2Settings where
  parseJSON =
    withObject
      "IBMDB2Settings"
      ( \x ->
          IBMDB2Settings'
            <$> (x .:? "ServerName")
            <*> (x .:? "CurrentLsn")
            <*> (x .:? "SetDataCaptureChanges")
            <*> (x .:? "Username")
            <*> (x .:? "Password")
            <*> (x .:? "DatabaseName")
            <*> (x .:? "MaxKBytesPerRead")
            <*> (x .:? "Port")
      )

instance Hashable IBMDB2Settings

instance NFData IBMDB2Settings

instance ToJSON IBMDB2Settings where
  toJSON IBMDB2Settings' {..} =
    object
      ( catMaybes
          [ ("ServerName" .=) <$> _ibmdsServerName,
            ("CurrentLsn" .=) <$> _ibmdsCurrentLsn,
            ("SetDataCaptureChanges" .=) <$> _ibmdsSetDataCaptureChanges,
            ("Username" .=) <$> _ibmdsUsername,
            ("Password" .=) <$> _ibmdsPassword,
            ("DatabaseName" .=) <$> _ibmdsDatabaseName,
            ("MaxKBytesPerRead" .=) <$> _ibmdsMaxKBytesPerRead,
            ("Port" .=) <$> _ibmdsPort
          ]
      )

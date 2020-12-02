{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.MicrosoftSQLServerSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.MicrosoftSQLServerSettings where

import Network.AWS.DMS.Types.SafeguardPolicy
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides information that defines a Microsoft SQL Server endpoint.
--
--
--
-- /See:/ 'microsoftSQLServerSettings' smart constructor.
data MicrosoftSQLServerSettings = MicrosoftSQLServerSettings'
  { _msqlssBcpPacketSize ::
      !(Maybe Int),
    _msqlssUseBcpFullLoad ::
      !(Maybe Bool),
    _msqlssServerName :: !(Maybe Text),
    _msqlssUsername :: !(Maybe Text),
    _msqlssSafeguardPolicy ::
      !(Maybe SafeguardPolicy),
    _msqlssPassword ::
      !(Maybe (Sensitive Text)),
    _msqlssDatabaseName :: !(Maybe Text),
    _msqlssReadBackupOnly ::
      !(Maybe Bool),
    _msqlssControlTablesFileGroup ::
      !(Maybe Text),
    _msqlssPort :: !(Maybe Int)
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'MicrosoftSQLServerSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'msqlssBcpPacketSize' - The maximum size of the packets (in bytes) used to transfer data using BCP.
--
-- * 'msqlssUseBcpFullLoad' - Use this to attribute to transfer data for full-load operations using BCP. When the target table contains an identity column that does not exist in the source table, you must disable the use BCP for loading table option.
--
-- * 'msqlssServerName' - Fully qualified domain name of the endpoint.
--
-- * 'msqlssUsername' - Endpoint connection user name.
--
-- * 'msqlssSafeguardPolicy' - Use this attribute to minimize the need to access the backup log and enable AWS DMS to prevent truncation using one of the following two methods. /Start transactions in the database:/ This is the default method. When this method is used, AWS DMS prevents TLOG truncation by mimicking a transaction in the database. As long as such a transaction is open, changes that appear after the transaction started aren't truncated. If you need Microsoft Replication to be enabled in your database, then you must choose this method. /Exclusively use sp_repldone within a single task/ : When this method is used, AWS DMS reads the changes and then uses sp_repldone to mark the TLOG transactions as ready for truncation. Although this method doesn't involve any transactional activities, it can only be used when Microsoft Replication isn't running. Also, when using this method, only one AWS DMS task can access the database at any given time. Therefore, if you need to run parallel AWS DMS tasks against the same database, use the default method.
--
-- * 'msqlssPassword' - Endpoint connection password.
--
-- * 'msqlssDatabaseName' - Database name for the endpoint.
--
-- * 'msqlssReadBackupOnly' - When this attribute is set to @Y@ , AWS DMS only reads changes from transaction log backups and doesn't read from the active transaction log file during ongoing replication. Setting this parameter to @Y@ enables you to control active transaction log file growth during full load and ongoing replication tasks. However, it can add some source latency to ongoing replication.
--
-- * 'msqlssControlTablesFileGroup' - Specify a filegroup for the AWS DMS internal tables. When the replication task starts, all the internal AWS DMS control tables (awsdms_ apply_exception, awsdms_apply, awsdms_changes) are created on the specified filegroup.
--
-- * 'msqlssPort' - Endpoint TCP port.
microsoftSQLServerSettings ::
  MicrosoftSQLServerSettings
microsoftSQLServerSettings =
  MicrosoftSQLServerSettings'
    { _msqlssBcpPacketSize = Nothing,
      _msqlssUseBcpFullLoad = Nothing,
      _msqlssServerName = Nothing,
      _msqlssUsername = Nothing,
      _msqlssSafeguardPolicy = Nothing,
      _msqlssPassword = Nothing,
      _msqlssDatabaseName = Nothing,
      _msqlssReadBackupOnly = Nothing,
      _msqlssControlTablesFileGroup = Nothing,
      _msqlssPort = Nothing
    }

-- | The maximum size of the packets (in bytes) used to transfer data using BCP.
msqlssBcpPacketSize :: Lens' MicrosoftSQLServerSettings (Maybe Int)
msqlssBcpPacketSize = lens _msqlssBcpPacketSize (\s a -> s {_msqlssBcpPacketSize = a})

-- | Use this to attribute to transfer data for full-load operations using BCP. When the target table contains an identity column that does not exist in the source table, you must disable the use BCP for loading table option.
msqlssUseBcpFullLoad :: Lens' MicrosoftSQLServerSettings (Maybe Bool)
msqlssUseBcpFullLoad = lens _msqlssUseBcpFullLoad (\s a -> s {_msqlssUseBcpFullLoad = a})

-- | Fully qualified domain name of the endpoint.
msqlssServerName :: Lens' MicrosoftSQLServerSettings (Maybe Text)
msqlssServerName = lens _msqlssServerName (\s a -> s {_msqlssServerName = a})

-- | Endpoint connection user name.
msqlssUsername :: Lens' MicrosoftSQLServerSettings (Maybe Text)
msqlssUsername = lens _msqlssUsername (\s a -> s {_msqlssUsername = a})

-- | Use this attribute to minimize the need to access the backup log and enable AWS DMS to prevent truncation using one of the following two methods. /Start transactions in the database:/ This is the default method. When this method is used, AWS DMS prevents TLOG truncation by mimicking a transaction in the database. As long as such a transaction is open, changes that appear after the transaction started aren't truncated. If you need Microsoft Replication to be enabled in your database, then you must choose this method. /Exclusively use sp_repldone within a single task/ : When this method is used, AWS DMS reads the changes and then uses sp_repldone to mark the TLOG transactions as ready for truncation. Although this method doesn't involve any transactional activities, it can only be used when Microsoft Replication isn't running. Also, when using this method, only one AWS DMS task can access the database at any given time. Therefore, if you need to run parallel AWS DMS tasks against the same database, use the default method.
msqlssSafeguardPolicy :: Lens' MicrosoftSQLServerSettings (Maybe SafeguardPolicy)
msqlssSafeguardPolicy = lens _msqlssSafeguardPolicy (\s a -> s {_msqlssSafeguardPolicy = a})

-- | Endpoint connection password.
msqlssPassword :: Lens' MicrosoftSQLServerSettings (Maybe Text)
msqlssPassword = lens _msqlssPassword (\s a -> s {_msqlssPassword = a}) . mapping _Sensitive

-- | Database name for the endpoint.
msqlssDatabaseName :: Lens' MicrosoftSQLServerSettings (Maybe Text)
msqlssDatabaseName = lens _msqlssDatabaseName (\s a -> s {_msqlssDatabaseName = a})

-- | When this attribute is set to @Y@ , AWS DMS only reads changes from transaction log backups and doesn't read from the active transaction log file during ongoing replication. Setting this parameter to @Y@ enables you to control active transaction log file growth during full load and ongoing replication tasks. However, it can add some source latency to ongoing replication.
msqlssReadBackupOnly :: Lens' MicrosoftSQLServerSettings (Maybe Bool)
msqlssReadBackupOnly = lens _msqlssReadBackupOnly (\s a -> s {_msqlssReadBackupOnly = a})

-- | Specify a filegroup for the AWS DMS internal tables. When the replication task starts, all the internal AWS DMS control tables (awsdms_ apply_exception, awsdms_apply, awsdms_changes) are created on the specified filegroup.
msqlssControlTablesFileGroup :: Lens' MicrosoftSQLServerSettings (Maybe Text)
msqlssControlTablesFileGroup = lens _msqlssControlTablesFileGroup (\s a -> s {_msqlssControlTablesFileGroup = a})

-- | Endpoint TCP port.
msqlssPort :: Lens' MicrosoftSQLServerSettings (Maybe Int)
msqlssPort = lens _msqlssPort (\s a -> s {_msqlssPort = a})

instance FromJSON MicrosoftSQLServerSettings where
  parseJSON =
    withObject
      "MicrosoftSQLServerSettings"
      ( \x ->
          MicrosoftSQLServerSettings'
            <$> (x .:? "BcpPacketSize")
            <*> (x .:? "UseBcpFullLoad")
            <*> (x .:? "ServerName")
            <*> (x .:? "Username")
            <*> (x .:? "SafeguardPolicy")
            <*> (x .:? "Password")
            <*> (x .:? "DatabaseName")
            <*> (x .:? "ReadBackupOnly")
            <*> (x .:? "ControlTablesFileGroup")
            <*> (x .:? "Port")
      )

instance Hashable MicrosoftSQLServerSettings

instance NFData MicrosoftSQLServerSettings

instance ToJSON MicrosoftSQLServerSettings where
  toJSON MicrosoftSQLServerSettings' {..} =
    object
      ( catMaybes
          [ ("BcpPacketSize" .=) <$> _msqlssBcpPacketSize,
            ("UseBcpFullLoad" .=) <$> _msqlssUseBcpFullLoad,
            ("ServerName" .=) <$> _msqlssServerName,
            ("Username" .=) <$> _msqlssUsername,
            ("SafeguardPolicy" .=) <$> _msqlssSafeguardPolicy,
            ("Password" .=) <$> _msqlssPassword,
            ("DatabaseName" .=) <$> _msqlssDatabaseName,
            ("ReadBackupOnly" .=) <$> _msqlssReadBackupOnly,
            ("ControlTablesFileGroup" .=) <$> _msqlssControlTablesFileGroup,
            ("Port" .=) <$> _msqlssPort
          ]
      )

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.PostgreSQLSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.PostgreSQLSettings where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides information that defines a PostgreSQL endpoint.
--
--
--
-- /See:/ 'postgreSQLSettings' smart constructor.
data PostgreSQLSettings = PostgreSQLSettings'
  { _psqlsExecuteTimeout ::
      !(Maybe Int),
    _psqlsMaxFileSize :: !(Maybe Int),
    _psqlsFailTasksOnLobTruncation :: !(Maybe Bool),
    _psqlsServerName :: !(Maybe Text),
    _psqlsDdlArtifactsSchema :: !(Maybe Text),
    _psqlsSlotName :: !(Maybe Text),
    _psqlsUsername :: !(Maybe Text),
    _psqlsPassword :: !(Maybe (Sensitive Text)),
    _psqlsDatabaseName :: !(Maybe Text),
    _psqlsAfterConnectScript :: !(Maybe Text),
    _psqlsCaptureDdls :: !(Maybe Bool),
    _psqlsPort :: !(Maybe Int)
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'PostgreSQLSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'psqlsExecuteTimeout' - Sets the client statement timeout for the PostgreSQL instance, in seconds. The default value is 60 seconds. Example: @executeTimeout=100;@
--
-- * 'psqlsMaxFileSize' - Specifies the maximum size (in KB) of any .csv file used to transfer data to PostgreSQL. Example: @maxFileSize=512@
--
-- * 'psqlsFailTasksOnLobTruncation' - When set to @true@ , this value causes a task to fail if the actual size of a LOB column is greater than the specified @LobMaxSize@ . If task is set to Limited LOB mode and this option is set to true, the task fails instead of truncating the LOB data.
--
-- * 'psqlsServerName' - Fully qualified domain name of the endpoint.
--
-- * 'psqlsDdlArtifactsSchema' - The schema in which the operational DDL database artifacts are created. Example: @ddlArtifactsSchema=xyzddlschema;@
--
-- * 'psqlsSlotName' - Sets the name of a previously created logical replication slot for a CDC load of the PostgreSQL source instance. When used with the AWS DMS API @CdcStartPosition@ request parameter, this attribute also enables using native CDC start points.
--
-- * 'psqlsUsername' - Endpoint connection user name.
--
-- * 'psqlsPassword' - Endpoint connection password.
--
-- * 'psqlsDatabaseName' - Database name for the endpoint.
--
-- * 'psqlsAfterConnectScript' - For use with change data capture (CDC) only, this attribute has AWS DMS bypass foreign keys and user triggers to reduce the time it takes to bulk load data. Example: @afterConnectScript=SET session_replication_role='replica'@
--
-- * 'psqlsCaptureDdls' - To capture DDL events, AWS DMS creates various artifacts in the PostgreSQL database when the task starts. You can later remove these artifacts. If this value is set to @N@ , you don't have to create tables or triggers on the source database.
--
-- * 'psqlsPort' - Endpoint TCP port.
postgreSQLSettings ::
  PostgreSQLSettings
postgreSQLSettings =
  PostgreSQLSettings'
    { _psqlsExecuteTimeout = Nothing,
      _psqlsMaxFileSize = Nothing,
      _psqlsFailTasksOnLobTruncation = Nothing,
      _psqlsServerName = Nothing,
      _psqlsDdlArtifactsSchema = Nothing,
      _psqlsSlotName = Nothing,
      _psqlsUsername = Nothing,
      _psqlsPassword = Nothing,
      _psqlsDatabaseName = Nothing,
      _psqlsAfterConnectScript = Nothing,
      _psqlsCaptureDdls = Nothing,
      _psqlsPort = Nothing
    }

-- | Sets the client statement timeout for the PostgreSQL instance, in seconds. The default value is 60 seconds. Example: @executeTimeout=100;@
psqlsExecuteTimeout :: Lens' PostgreSQLSettings (Maybe Int)
psqlsExecuteTimeout = lens _psqlsExecuteTimeout (\s a -> s {_psqlsExecuteTimeout = a})

-- | Specifies the maximum size (in KB) of any .csv file used to transfer data to PostgreSQL. Example: @maxFileSize=512@
psqlsMaxFileSize :: Lens' PostgreSQLSettings (Maybe Int)
psqlsMaxFileSize = lens _psqlsMaxFileSize (\s a -> s {_psqlsMaxFileSize = a})

-- | When set to @true@ , this value causes a task to fail if the actual size of a LOB column is greater than the specified @LobMaxSize@ . If task is set to Limited LOB mode and this option is set to true, the task fails instead of truncating the LOB data.
psqlsFailTasksOnLobTruncation :: Lens' PostgreSQLSettings (Maybe Bool)
psqlsFailTasksOnLobTruncation = lens _psqlsFailTasksOnLobTruncation (\s a -> s {_psqlsFailTasksOnLobTruncation = a})

-- | Fully qualified domain name of the endpoint.
psqlsServerName :: Lens' PostgreSQLSettings (Maybe Text)
psqlsServerName = lens _psqlsServerName (\s a -> s {_psqlsServerName = a})

-- | The schema in which the operational DDL database artifacts are created. Example: @ddlArtifactsSchema=xyzddlschema;@
psqlsDdlArtifactsSchema :: Lens' PostgreSQLSettings (Maybe Text)
psqlsDdlArtifactsSchema = lens _psqlsDdlArtifactsSchema (\s a -> s {_psqlsDdlArtifactsSchema = a})

-- | Sets the name of a previously created logical replication slot for a CDC load of the PostgreSQL source instance. When used with the AWS DMS API @CdcStartPosition@ request parameter, this attribute also enables using native CDC start points.
psqlsSlotName :: Lens' PostgreSQLSettings (Maybe Text)
psqlsSlotName = lens _psqlsSlotName (\s a -> s {_psqlsSlotName = a})

-- | Endpoint connection user name.
psqlsUsername :: Lens' PostgreSQLSettings (Maybe Text)
psqlsUsername = lens _psqlsUsername (\s a -> s {_psqlsUsername = a})

-- | Endpoint connection password.
psqlsPassword :: Lens' PostgreSQLSettings (Maybe Text)
psqlsPassword = lens _psqlsPassword (\s a -> s {_psqlsPassword = a}) . mapping _Sensitive

-- | Database name for the endpoint.
psqlsDatabaseName :: Lens' PostgreSQLSettings (Maybe Text)
psqlsDatabaseName = lens _psqlsDatabaseName (\s a -> s {_psqlsDatabaseName = a})

-- | For use with change data capture (CDC) only, this attribute has AWS DMS bypass foreign keys and user triggers to reduce the time it takes to bulk load data. Example: @afterConnectScript=SET session_replication_role='replica'@
psqlsAfterConnectScript :: Lens' PostgreSQLSettings (Maybe Text)
psqlsAfterConnectScript = lens _psqlsAfterConnectScript (\s a -> s {_psqlsAfterConnectScript = a})

-- | To capture DDL events, AWS DMS creates various artifacts in the PostgreSQL database when the task starts. You can later remove these artifacts. If this value is set to @N@ , you don't have to create tables or triggers on the source database.
psqlsCaptureDdls :: Lens' PostgreSQLSettings (Maybe Bool)
psqlsCaptureDdls = lens _psqlsCaptureDdls (\s a -> s {_psqlsCaptureDdls = a})

-- | Endpoint TCP port.
psqlsPort :: Lens' PostgreSQLSettings (Maybe Int)
psqlsPort = lens _psqlsPort (\s a -> s {_psqlsPort = a})

instance FromJSON PostgreSQLSettings where
  parseJSON =
    withObject
      "PostgreSQLSettings"
      ( \x ->
          PostgreSQLSettings'
            <$> (x .:? "ExecuteTimeout")
            <*> (x .:? "MaxFileSize")
            <*> (x .:? "FailTasksOnLobTruncation")
            <*> (x .:? "ServerName")
            <*> (x .:? "DdlArtifactsSchema")
            <*> (x .:? "SlotName")
            <*> (x .:? "Username")
            <*> (x .:? "Password")
            <*> (x .:? "DatabaseName")
            <*> (x .:? "AfterConnectScript")
            <*> (x .:? "CaptureDdls")
            <*> (x .:? "Port")
      )

instance Hashable PostgreSQLSettings

instance NFData PostgreSQLSettings

instance ToJSON PostgreSQLSettings where
  toJSON PostgreSQLSettings' {..} =
    object
      ( catMaybes
          [ ("ExecuteTimeout" .=) <$> _psqlsExecuteTimeout,
            ("MaxFileSize" .=) <$> _psqlsMaxFileSize,
            ("FailTasksOnLobTruncation" .=) <$> _psqlsFailTasksOnLobTruncation,
            ("ServerName" .=) <$> _psqlsServerName,
            ("DdlArtifactsSchema" .=) <$> _psqlsDdlArtifactsSchema,
            ("SlotName" .=) <$> _psqlsSlotName,
            ("Username" .=) <$> _psqlsUsername,
            ("Password" .=) <$> _psqlsPassword,
            ("DatabaseName" .=) <$> _psqlsDatabaseName,
            ("AfterConnectScript" .=) <$> _psqlsAfterConnectScript,
            ("CaptureDdls" .=) <$> _psqlsCaptureDdls,
            ("Port" .=) <$> _psqlsPort
          ]
      )

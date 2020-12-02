{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.MySQLSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.MySQLSettings where

import Network.AWS.DMS.Types.TargetDBType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides information that defines a MySQL endpoint.
--
--
--
-- /See:/ 'mySQLSettings' smart constructor.
data MySQLSettings = MySQLSettings'
  { _msqlsMaxFileSize ::
      !(Maybe Int),
    _msqlsTargetDBType :: !(Maybe TargetDBType),
    _msqlsServerName :: !(Maybe Text),
    _msqlsParallelLoadThreads :: !(Maybe Int),
    _msqlsUsername :: !(Maybe Text),
    _msqlsPassword :: !(Maybe (Sensitive Text)),
    _msqlsEventsPollInterval :: !(Maybe Int),
    _msqlsDatabaseName :: !(Maybe Text),
    _msqlsAfterConnectScript :: !(Maybe Text),
    _msqlsServerTimezone :: !(Maybe Text),
    _msqlsPort :: !(Maybe Int)
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'MySQLSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'msqlsMaxFileSize' - Specifies the maximum size (in KB) of any .csv file used to transfer data to a MySQL-compatible database. Example: @maxFileSize=512@
--
-- * 'msqlsTargetDBType' - Specifies where to migrate source tables on the target, either to a single database or multiple databases. Example: @targetDbType=MULTIPLE_DATABASES@
--
-- * 'msqlsServerName' - Fully qualified domain name of the endpoint.
--
-- * 'msqlsParallelLoadThreads' - Improves performance when loading data into the MySQLcompatible target database. Specifies how many threads to use to load the data into the MySQL-compatible target database. Setting a large number of threads can have an adverse effect on database performance, because a separate connection is required for each thread. Example: @parallelLoadThreads=1@
--
-- * 'msqlsUsername' - Endpoint connection user name.
--
-- * 'msqlsPassword' - Endpoint connection password.
--
-- * 'msqlsEventsPollInterval' - Specifies how often to check the binary log for new changes/events when the database is idle. Example: @eventsPollInterval=5;@  In the example, AWS DMS checks for changes in the binary logs every five seconds.
--
-- * 'msqlsDatabaseName' - Database name for the endpoint.
--
-- * 'msqlsAfterConnectScript' - Specifies a script to run immediately after AWS DMS connects to the endpoint. The migration task continues running regardless if the SQL statement succeeds or fails.
--
-- * 'msqlsServerTimezone' - Specifies the time zone for the source MySQL database. Example: @serverTimezone=US/Pacific;@  Note: Do not enclose time zones in single quotes.
--
-- * 'msqlsPort' - Endpoint TCP port.
mySQLSettings ::
  MySQLSettings
mySQLSettings =
  MySQLSettings'
    { _msqlsMaxFileSize = Nothing,
      _msqlsTargetDBType = Nothing,
      _msqlsServerName = Nothing,
      _msqlsParallelLoadThreads = Nothing,
      _msqlsUsername = Nothing,
      _msqlsPassword = Nothing,
      _msqlsEventsPollInterval = Nothing,
      _msqlsDatabaseName = Nothing,
      _msqlsAfterConnectScript = Nothing,
      _msqlsServerTimezone = Nothing,
      _msqlsPort = Nothing
    }

-- | Specifies the maximum size (in KB) of any .csv file used to transfer data to a MySQL-compatible database. Example: @maxFileSize=512@
msqlsMaxFileSize :: Lens' MySQLSettings (Maybe Int)
msqlsMaxFileSize = lens _msqlsMaxFileSize (\s a -> s {_msqlsMaxFileSize = a})

-- | Specifies where to migrate source tables on the target, either to a single database or multiple databases. Example: @targetDbType=MULTIPLE_DATABASES@
msqlsTargetDBType :: Lens' MySQLSettings (Maybe TargetDBType)
msqlsTargetDBType = lens _msqlsTargetDBType (\s a -> s {_msqlsTargetDBType = a})

-- | Fully qualified domain name of the endpoint.
msqlsServerName :: Lens' MySQLSettings (Maybe Text)
msqlsServerName = lens _msqlsServerName (\s a -> s {_msqlsServerName = a})

-- | Improves performance when loading data into the MySQLcompatible target database. Specifies how many threads to use to load the data into the MySQL-compatible target database. Setting a large number of threads can have an adverse effect on database performance, because a separate connection is required for each thread. Example: @parallelLoadThreads=1@
msqlsParallelLoadThreads :: Lens' MySQLSettings (Maybe Int)
msqlsParallelLoadThreads = lens _msqlsParallelLoadThreads (\s a -> s {_msqlsParallelLoadThreads = a})

-- | Endpoint connection user name.
msqlsUsername :: Lens' MySQLSettings (Maybe Text)
msqlsUsername = lens _msqlsUsername (\s a -> s {_msqlsUsername = a})

-- | Endpoint connection password.
msqlsPassword :: Lens' MySQLSettings (Maybe Text)
msqlsPassword = lens _msqlsPassword (\s a -> s {_msqlsPassword = a}) . mapping _Sensitive

-- | Specifies how often to check the binary log for new changes/events when the database is idle. Example: @eventsPollInterval=5;@  In the example, AWS DMS checks for changes in the binary logs every five seconds.
msqlsEventsPollInterval :: Lens' MySQLSettings (Maybe Int)
msqlsEventsPollInterval = lens _msqlsEventsPollInterval (\s a -> s {_msqlsEventsPollInterval = a})

-- | Database name for the endpoint.
msqlsDatabaseName :: Lens' MySQLSettings (Maybe Text)
msqlsDatabaseName = lens _msqlsDatabaseName (\s a -> s {_msqlsDatabaseName = a})

-- | Specifies a script to run immediately after AWS DMS connects to the endpoint. The migration task continues running regardless if the SQL statement succeeds or fails.
msqlsAfterConnectScript :: Lens' MySQLSettings (Maybe Text)
msqlsAfterConnectScript = lens _msqlsAfterConnectScript (\s a -> s {_msqlsAfterConnectScript = a})

-- | Specifies the time zone for the source MySQL database. Example: @serverTimezone=US/Pacific;@  Note: Do not enclose time zones in single quotes.
msqlsServerTimezone :: Lens' MySQLSettings (Maybe Text)
msqlsServerTimezone = lens _msqlsServerTimezone (\s a -> s {_msqlsServerTimezone = a})

-- | Endpoint TCP port.
msqlsPort :: Lens' MySQLSettings (Maybe Int)
msqlsPort = lens _msqlsPort (\s a -> s {_msqlsPort = a})

instance FromJSON MySQLSettings where
  parseJSON =
    withObject
      "MySQLSettings"
      ( \x ->
          MySQLSettings'
            <$> (x .:? "MaxFileSize")
            <*> (x .:? "TargetDbType")
            <*> (x .:? "ServerName")
            <*> (x .:? "ParallelLoadThreads")
            <*> (x .:? "Username")
            <*> (x .:? "Password")
            <*> (x .:? "EventsPollInterval")
            <*> (x .:? "DatabaseName")
            <*> (x .:? "AfterConnectScript")
            <*> (x .:? "ServerTimezone")
            <*> (x .:? "Port")
      )

instance Hashable MySQLSettings

instance NFData MySQLSettings

instance ToJSON MySQLSettings where
  toJSON MySQLSettings' {..} =
    object
      ( catMaybes
          [ ("MaxFileSize" .=) <$> _msqlsMaxFileSize,
            ("TargetDbType" .=) <$> _msqlsTargetDBType,
            ("ServerName" .=) <$> _msqlsServerName,
            ("ParallelLoadThreads" .=) <$> _msqlsParallelLoadThreads,
            ("Username" .=) <$> _msqlsUsername,
            ("Password" .=) <$> _msqlsPassword,
            ("EventsPollInterval" .=) <$> _msqlsEventsPollInterval,
            ("DatabaseName" .=) <$> _msqlsDatabaseName,
            ("AfterConnectScript" .=) <$> _msqlsAfterConnectScript,
            ("ServerTimezone" .=) <$> _msqlsServerTimezone,
            ("Port" .=) <$> _msqlsPort
          ]
      )

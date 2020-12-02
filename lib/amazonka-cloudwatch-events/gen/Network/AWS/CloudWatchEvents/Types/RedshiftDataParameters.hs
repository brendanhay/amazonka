{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types.RedshiftDataParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.RedshiftDataParameters where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | These are custom parameters to be used when the target is a Redshift cluster to invoke the Redshift Data API ExecuteStatement based on EventBridge events.
--
--
--
-- /See:/ 'redshiftDataParameters' smart constructor.
data RedshiftDataParameters = RedshiftDataParameters'
  { _rdpDBUser ::
      !(Maybe Text),
    _rdpSecretManagerARN :: !(Maybe Text),
    _rdpStatementName :: !(Maybe Text),
    _rdpWithEvent :: !(Maybe Bool),
    _rdpDatabase :: !Text,
    _rdpSql :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RedshiftDataParameters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdpDBUser' - The database user name. Required when authenticating using temporary credentials.
--
-- * 'rdpSecretManagerARN' - The name or ARN of the secret that enables access to the database. Required when authenticating using AWS Secrets Manager.
--
-- * 'rdpStatementName' - The name of the SQL statement. You can name the SQL statement when you create it to identify the query.
--
-- * 'rdpWithEvent' - Indicates whether to send an event back to EventBridge after the SQL statement runs.
--
-- * 'rdpDatabase' - The name of the database. Required when authenticating using temporary credentials.
--
-- * 'rdpSql' - The SQL statement text to run.
redshiftDataParameters ::
  -- | 'rdpDatabase'
  Text ->
  -- | 'rdpSql'
  Text ->
  RedshiftDataParameters
redshiftDataParameters pDatabase_ pSql_ =
  RedshiftDataParameters'
    { _rdpDBUser = Nothing,
      _rdpSecretManagerARN = Nothing,
      _rdpStatementName = Nothing,
      _rdpWithEvent = Nothing,
      _rdpDatabase = pDatabase_,
      _rdpSql = pSql_
    }

-- | The database user name. Required when authenticating using temporary credentials.
rdpDBUser :: Lens' RedshiftDataParameters (Maybe Text)
rdpDBUser = lens _rdpDBUser (\s a -> s {_rdpDBUser = a})

-- | The name or ARN of the secret that enables access to the database. Required when authenticating using AWS Secrets Manager.
rdpSecretManagerARN :: Lens' RedshiftDataParameters (Maybe Text)
rdpSecretManagerARN = lens _rdpSecretManagerARN (\s a -> s {_rdpSecretManagerARN = a})

-- | The name of the SQL statement. You can name the SQL statement when you create it to identify the query.
rdpStatementName :: Lens' RedshiftDataParameters (Maybe Text)
rdpStatementName = lens _rdpStatementName (\s a -> s {_rdpStatementName = a})

-- | Indicates whether to send an event back to EventBridge after the SQL statement runs.
rdpWithEvent :: Lens' RedshiftDataParameters (Maybe Bool)
rdpWithEvent = lens _rdpWithEvent (\s a -> s {_rdpWithEvent = a})

-- | The name of the database. Required when authenticating using temporary credentials.
rdpDatabase :: Lens' RedshiftDataParameters Text
rdpDatabase = lens _rdpDatabase (\s a -> s {_rdpDatabase = a})

-- | The SQL statement text to run.
rdpSql :: Lens' RedshiftDataParameters Text
rdpSql = lens _rdpSql (\s a -> s {_rdpSql = a})

instance FromJSON RedshiftDataParameters where
  parseJSON =
    withObject
      "RedshiftDataParameters"
      ( \x ->
          RedshiftDataParameters'
            <$> (x .:? "DbUser")
            <*> (x .:? "SecretManagerArn")
            <*> (x .:? "StatementName")
            <*> (x .:? "WithEvent")
            <*> (x .: "Database")
            <*> (x .: "Sql")
      )

instance Hashable RedshiftDataParameters

instance NFData RedshiftDataParameters

instance ToJSON RedshiftDataParameters where
  toJSON RedshiftDataParameters' {..} =
    object
      ( catMaybes
          [ ("DbUser" .=) <$> _rdpDBUser,
            ("SecretManagerArn" .=) <$> _rdpSecretManagerARN,
            ("StatementName" .=) <$> _rdpStatementName,
            ("WithEvent" .=) <$> _rdpWithEvent,
            Just ("Database" .= _rdpDatabase),
            Just ("Sql" .= _rdpSql)
          ]
      )

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.Types.RedshiftMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MachineLearning.Types.RedshiftMetadata where

import Network.AWS.Lens
import Network.AWS.MachineLearning.Types.RedshiftDatabase
import Network.AWS.Prelude

-- | Describes the @DataSource@ details specific to Amazon Redshift.
--
--
--
-- /See:/ 'redshiftMetadata' smart constructor.
data RedshiftMetadata = RedshiftMetadata'
  { _redSelectSqlQuery ::
      !(Maybe Text),
    _redRedshiftDatabase :: !(Maybe RedshiftDatabase),
    _redDatabaseUserName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RedshiftMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'redSelectSqlQuery' - The SQL query that is specified during 'CreateDataSourceFromRedshift' . Returns only if @Verbose@ is true in GetDataSourceInput.
--
-- * 'redRedshiftDatabase' - Undocumented member.
--
-- * 'redDatabaseUserName' - Undocumented member.
redshiftMetadata ::
  RedshiftMetadata
redshiftMetadata =
  RedshiftMetadata'
    { _redSelectSqlQuery = Nothing,
      _redRedshiftDatabase = Nothing,
      _redDatabaseUserName = Nothing
    }

-- | The SQL query that is specified during 'CreateDataSourceFromRedshift' . Returns only if @Verbose@ is true in GetDataSourceInput.
redSelectSqlQuery :: Lens' RedshiftMetadata (Maybe Text)
redSelectSqlQuery = lens _redSelectSqlQuery (\s a -> s {_redSelectSqlQuery = a})

-- | Undocumented member.
redRedshiftDatabase :: Lens' RedshiftMetadata (Maybe RedshiftDatabase)
redRedshiftDatabase = lens _redRedshiftDatabase (\s a -> s {_redRedshiftDatabase = a})

-- | Undocumented member.
redDatabaseUserName :: Lens' RedshiftMetadata (Maybe Text)
redDatabaseUserName = lens _redDatabaseUserName (\s a -> s {_redDatabaseUserName = a})

instance FromJSON RedshiftMetadata where
  parseJSON =
    withObject
      "RedshiftMetadata"
      ( \x ->
          RedshiftMetadata'
            <$> (x .:? "SelectSqlQuery")
            <*> (x .:? "RedshiftDatabase")
            <*> (x .:? "DatabaseUserName")
      )

instance Hashable RedshiftMetadata

instance NFData RedshiftMetadata

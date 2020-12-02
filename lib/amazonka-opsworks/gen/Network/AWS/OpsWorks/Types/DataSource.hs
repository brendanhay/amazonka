{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.DataSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.DataSource where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an app's data source.
--
--
--
-- /See:/ 'dataSource' smart constructor.
data DataSource = DataSource'
  { _dsARN :: !(Maybe Text),
    _dsDatabaseName :: !(Maybe Text),
    _dsType :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DataSource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsARN' - The data source's ARN.
--
-- * 'dsDatabaseName' - The database name.
--
-- * 'dsType' - The data source's type, @AutoSelectOpsworksMysqlInstance@ , @OpsworksMysqlInstance@ , @RdsDbInstance@ , or @None@ .
dataSource ::
  DataSource
dataSource =
  DataSource'
    { _dsARN = Nothing,
      _dsDatabaseName = Nothing,
      _dsType = Nothing
    }

-- | The data source's ARN.
dsARN :: Lens' DataSource (Maybe Text)
dsARN = lens _dsARN (\s a -> s {_dsARN = a})

-- | The database name.
dsDatabaseName :: Lens' DataSource (Maybe Text)
dsDatabaseName = lens _dsDatabaseName (\s a -> s {_dsDatabaseName = a})

-- | The data source's type, @AutoSelectOpsworksMysqlInstance@ , @OpsworksMysqlInstance@ , @RdsDbInstance@ , or @None@ .
dsType :: Lens' DataSource (Maybe Text)
dsType = lens _dsType (\s a -> s {_dsType = a})

instance FromJSON DataSource where
  parseJSON =
    withObject
      "DataSource"
      ( \x ->
          DataSource'
            <$> (x .:? "Arn") <*> (x .:? "DatabaseName") <*> (x .:? "Type")
      )

instance Hashable DataSource

instance NFData DataSource

instance ToJSON DataSource where
  toJSON DataSource' {..} =
    object
      ( catMaybes
          [ ("Arn" .=) <$> _dsARN,
            ("DatabaseName" .=) <$> _dsDatabaseName,
            ("Type" .=) <$> _dsType
          ]
      )

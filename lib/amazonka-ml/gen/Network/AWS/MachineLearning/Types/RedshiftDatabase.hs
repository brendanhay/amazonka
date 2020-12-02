{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.Types.RedshiftDatabase
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MachineLearning.Types.RedshiftDatabase where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the database details required to connect to an Amazon Redshift database.
--
--
--
-- /See:/ 'redshiftDatabase' smart constructor.
data RedshiftDatabase = RedshiftDatabase'
  { _rdDatabaseName :: !Text,
    _rdClusterIdentifier :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RedshiftDatabase' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdDatabaseName' - Undocumented member.
--
-- * 'rdClusterIdentifier' - Undocumented member.
redshiftDatabase ::
  -- | 'rdDatabaseName'
  Text ->
  -- | 'rdClusterIdentifier'
  Text ->
  RedshiftDatabase
redshiftDatabase pDatabaseName_ pClusterIdentifier_ =
  RedshiftDatabase'
    { _rdDatabaseName = pDatabaseName_,
      _rdClusterIdentifier = pClusterIdentifier_
    }

-- | Undocumented member.
rdDatabaseName :: Lens' RedshiftDatabase Text
rdDatabaseName = lens _rdDatabaseName (\s a -> s {_rdDatabaseName = a})

-- | Undocumented member.
rdClusterIdentifier :: Lens' RedshiftDatabase Text
rdClusterIdentifier = lens _rdClusterIdentifier (\s a -> s {_rdClusterIdentifier = a})

instance FromJSON RedshiftDatabase where
  parseJSON =
    withObject
      "RedshiftDatabase"
      ( \x ->
          RedshiftDatabase'
            <$> (x .: "DatabaseName") <*> (x .: "ClusterIdentifier")
      )

instance Hashable RedshiftDatabase

instance NFData RedshiftDatabase

instance ToJSON RedshiftDatabase where
  toJSON RedshiftDatabase' {..} =
    object
      ( catMaybes
          [ Just ("DatabaseName" .= _rdDatabaseName),
            Just ("ClusterIdentifier" .= _rdClusterIdentifier)
          ]
      )

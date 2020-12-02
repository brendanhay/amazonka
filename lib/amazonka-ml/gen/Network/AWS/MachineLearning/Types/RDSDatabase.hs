{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.Types.RDSDatabase
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MachineLearning.Types.RDSDatabase where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The database details of an Amazon RDS database.
--
--
--
-- /See:/ 'rdsDatabase' smart constructor.
data RDSDatabase = RDSDatabase'
  { _rdsdInstanceIdentifier :: !Text,
    _rdsdDatabaseName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RDSDatabase' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdsdInstanceIdentifier' - The ID of an RDS DB instance.
--
-- * 'rdsdDatabaseName' - Undocumented member.
rdsDatabase ::
  -- | 'rdsdInstanceIdentifier'
  Text ->
  -- | 'rdsdDatabaseName'
  Text ->
  RDSDatabase
rdsDatabase pInstanceIdentifier_ pDatabaseName_ =
  RDSDatabase'
    { _rdsdInstanceIdentifier = pInstanceIdentifier_,
      _rdsdDatabaseName = pDatabaseName_
    }

-- | The ID of an RDS DB instance.
rdsdInstanceIdentifier :: Lens' RDSDatabase Text
rdsdInstanceIdentifier = lens _rdsdInstanceIdentifier (\s a -> s {_rdsdInstanceIdentifier = a})

-- | Undocumented member.
rdsdDatabaseName :: Lens' RDSDatabase Text
rdsdDatabaseName = lens _rdsdDatabaseName (\s a -> s {_rdsdDatabaseName = a})

instance FromJSON RDSDatabase where
  parseJSON =
    withObject
      "RDSDatabase"
      ( \x ->
          RDSDatabase'
            <$> (x .: "InstanceIdentifier") <*> (x .: "DatabaseName")
      )

instance Hashable RDSDatabase

instance NFData RDSDatabase

instance ToJSON RDSDatabase where
  toJSON RDSDatabase' {..} =
    object
      ( catMaybes
          [ Just ("InstanceIdentifier" .= _rdsdInstanceIdentifier),
            Just ("DatabaseName" .= _rdsdDatabaseName)
          ]
      )

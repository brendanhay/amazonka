{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.PendingModifiedRelationalDatabaseValues
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.PendingModifiedRelationalDatabaseValues where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a pending database value modification.
--
--
--
-- /See:/ 'pendingModifiedRelationalDatabaseValues' smart constructor.
data PendingModifiedRelationalDatabaseValues = PendingModifiedRelationalDatabaseValues'
  { _pmrdvEngineVersion ::
      !( Maybe
           Text
       ),
    _pmrdvMasterUserPassword ::
      !( Maybe
           Text
       ),
    _pmrdvBackupRetentionEnabled ::
      !( Maybe
           Bool
       )
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PendingModifiedRelationalDatabaseValues' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pmrdvEngineVersion' - The database engine version.
--
-- * 'pmrdvMasterUserPassword' - The password for the master user of the database.
--
-- * 'pmrdvBackupRetentionEnabled' - A Boolean value indicating whether automated backup retention is enabled.
pendingModifiedRelationalDatabaseValues ::
  PendingModifiedRelationalDatabaseValues
pendingModifiedRelationalDatabaseValues =
  PendingModifiedRelationalDatabaseValues'
    { _pmrdvEngineVersion =
        Nothing,
      _pmrdvMasterUserPassword = Nothing,
      _pmrdvBackupRetentionEnabled = Nothing
    }

-- | The database engine version.
pmrdvEngineVersion :: Lens' PendingModifiedRelationalDatabaseValues (Maybe Text)
pmrdvEngineVersion = lens _pmrdvEngineVersion (\s a -> s {_pmrdvEngineVersion = a})

-- | The password for the master user of the database.
pmrdvMasterUserPassword :: Lens' PendingModifiedRelationalDatabaseValues (Maybe Text)
pmrdvMasterUserPassword = lens _pmrdvMasterUserPassword (\s a -> s {_pmrdvMasterUserPassword = a})

-- | A Boolean value indicating whether automated backup retention is enabled.
pmrdvBackupRetentionEnabled :: Lens' PendingModifiedRelationalDatabaseValues (Maybe Bool)
pmrdvBackupRetentionEnabled = lens _pmrdvBackupRetentionEnabled (\s a -> s {_pmrdvBackupRetentionEnabled = a})

instance FromJSON PendingModifiedRelationalDatabaseValues where
  parseJSON =
    withObject
      "PendingModifiedRelationalDatabaseValues"
      ( \x ->
          PendingModifiedRelationalDatabaseValues'
            <$> (x .:? "engineVersion")
            <*> (x .:? "masterUserPassword")
            <*> (x .:? "backupRetentionEnabled")
      )

instance Hashable PendingModifiedRelationalDatabaseValues

instance NFData PendingModifiedRelationalDatabaseValues

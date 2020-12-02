{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.Types.BackupPolicyDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EFS.Types.BackupPolicyDescription where

import Network.AWS.EFS.Types.BackupPolicy
import Network.AWS.Lens
import Network.AWS.Prelude

-- | /See:/ 'backupPolicyDescription' smart constructor.
newtype BackupPolicyDescription = BackupPolicyDescription'
  { _bpdBackupPolicy ::
      Maybe BackupPolicy
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BackupPolicyDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bpdBackupPolicy' - Describes the file system's backup policy, indicating whether automatic backups are turned on or off..
backupPolicyDescription ::
  BackupPolicyDescription
backupPolicyDescription =
  BackupPolicyDescription' {_bpdBackupPolicy = Nothing}

-- | Describes the file system's backup policy, indicating whether automatic backups are turned on or off..
bpdBackupPolicy :: Lens' BackupPolicyDescription (Maybe BackupPolicy)
bpdBackupPolicy = lens _bpdBackupPolicy (\s a -> s {_bpdBackupPolicy = a})

instance FromJSON BackupPolicyDescription where
  parseJSON =
    withObject
      "BackupPolicyDescription"
      (\x -> BackupPolicyDescription' <$> (x .:? "BackupPolicy"))

instance Hashable BackupPolicyDescription

instance NFData BackupPolicyDescription

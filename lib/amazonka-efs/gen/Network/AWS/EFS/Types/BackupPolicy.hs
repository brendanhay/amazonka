{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.Types.BackupPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EFS.Types.BackupPolicy where

import Network.AWS.EFS.Types.Status
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The backup policy for the file system, showing the curent status. If @ENABLED@ , the file system is being backed up.
--
--
--
-- /See:/ 'backupPolicy' smart constructor.
newtype BackupPolicy = BackupPolicy' {_bpStatus :: Status}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BackupPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bpStatus' - Describes the status of the file system's backup policy.     * /@ENABLED@ - EFS is automatically backing up the file system./      * /@ENABLING@ - EFS is turning on automatic backups for the file system./      * /@DISABLED@ - automatic back ups are turned off for the file system./      * /@DISABLED@ - EFS is turning off automatic backups for the file system./
backupPolicy ::
  -- | 'bpStatus'
  Status ->
  BackupPolicy
backupPolicy pStatus_ = BackupPolicy' {_bpStatus = pStatus_}

-- | Describes the status of the file system's backup policy.     * /@ENABLED@ - EFS is automatically backing up the file system./      * /@ENABLING@ - EFS is turning on automatic backups for the file system./      * /@DISABLED@ - automatic back ups are turned off for the file system./      * /@DISABLED@ - EFS is turning off automatic backups for the file system./
bpStatus :: Lens' BackupPolicy Status
bpStatus = lens _bpStatus (\s a -> s {_bpStatus = a})

instance FromJSON BackupPolicy where
  parseJSON =
    withObject
      "BackupPolicy"
      (\x -> BackupPolicy' <$> (x .: "Status"))

instance Hashable BackupPolicy

instance NFData BackupPolicy

instance ToJSON BackupPolicy where
  toJSON BackupPolicy' {..} =
    object (catMaybes [Just ("Status" .= _bpStatus)])

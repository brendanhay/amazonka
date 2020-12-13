{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.Types.BackupPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EFS.Types.BackupPolicy
  ( BackupPolicy (..),

    -- * Smart constructor
    mkBackupPolicy,

    -- * Lenses
    bpStatus,
  )
where

import Network.AWS.EFS.Types.Status
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The backup policy for the file system, showing the curent status. If @ENABLED@ , the file system is being backed up.
--
-- /See:/ 'mkBackupPolicy' smart constructor.
newtype BackupPolicy = BackupPolicy'
  { -- | Describes the status of the file system's backup policy.
    --
    --
    --     * /@ENABLED@ - EFS is automatically backing up the file system./
    --
    --
    --     * /@ENABLING@ - EFS is turning on automatic backups for the file system./
    --
    --
    --     * /@DISABLED@ - automatic back ups are turned off for the file system./
    --
    --
    --     * /@DISABLED@ - EFS is turning off automatic backups for the file system./
    status :: Status
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BackupPolicy' with the minimum fields required to make a request.
--
-- * 'status' - Describes the status of the file system's backup policy.
--
--
--     * /@ENABLED@ - EFS is automatically backing up the file system./
--
--
--     * /@ENABLING@ - EFS is turning on automatic backups for the file system./
--
--
--     * /@DISABLED@ - automatic back ups are turned off for the file system./
--
--
--     * /@DISABLED@ - EFS is turning off automatic backups for the file system./
mkBackupPolicy ::
  -- | 'status'
  Status ->
  BackupPolicy
mkBackupPolicy pStatus_ = BackupPolicy' {status = pStatus_}

-- | Describes the status of the file system's backup policy.
--
--
--     * /@ENABLED@ - EFS is automatically backing up the file system./
--
--
--     * /@ENABLING@ - EFS is turning on automatic backups for the file system./
--
--
--     * /@DISABLED@ - automatic back ups are turned off for the file system./
--
--
--     * /@DISABLED@ - EFS is turning off automatic backups for the file system./
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpStatus :: Lens.Lens' BackupPolicy Status
bpStatus = Lens.lens (status :: BackupPolicy -> Status) (\s a -> s {status = a} :: BackupPolicy)
{-# DEPRECATED bpStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Lude.FromJSON BackupPolicy where
  parseJSON =
    Lude.withObject
      "BackupPolicy"
      (\x -> BackupPolicy' Lude.<$> (x Lude..: "Status"))

instance Lude.ToJSON BackupPolicy where
  toJSON BackupPolicy' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("Status" Lude..= status)])

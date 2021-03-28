{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.Types.BackupPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EFS.Types.BackupPolicy
  ( BackupPolicy (..)
  -- * Smart constructor
  , mkBackupPolicy
  -- * Lenses
  , bpStatus
  ) where

import qualified Network.AWS.EFS.Types.Status as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The backup policy for the file system, showing the curent status. If @ENABLED@ , the file system is being backed up.
--
-- /See:/ 'mkBackupPolicy' smart constructor.
newtype BackupPolicy = BackupPolicy'
  { status :: Types.Status
    -- ^ Describes the status of the file system's backup policy.
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
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'BackupPolicy' value with any optional fields omitted.
mkBackupPolicy
    :: Types.Status -- ^ 'status'
    -> BackupPolicy
mkBackupPolicy status = BackupPolicy'{status}

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
bpStatus :: Lens.Lens' BackupPolicy Types.Status
bpStatus = Lens.field @"status"
{-# INLINEABLE bpStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

instance Core.FromJSON BackupPolicy where
        toJSON BackupPolicy{..}
          = Core.object
              (Core.catMaybes [Core.Just ("Status" Core..= status)])

instance Core.FromJSON BackupPolicy where
        parseJSON
          = Core.withObject "BackupPolicy" Core.$
              \ x -> BackupPolicy' Core.<$> (x Core..: "Status")

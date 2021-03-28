{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.Types.BackupPolicyDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EFS.Types.BackupPolicyDescription
  ( BackupPolicyDescription (..)
  -- * Smart constructor
  , mkBackupPolicyDescription
  -- * Lenses
  , bpdBackupPolicy
  ) where

import qualified Network.AWS.EFS.Types.BackupPolicy as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | /See:/ 'mkBackupPolicyDescription' smart constructor.
newtype BackupPolicyDescription = BackupPolicyDescription'
  { backupPolicy :: Core.Maybe Types.BackupPolicy
    -- ^ Describes the file system's backup policy, indicating whether automatic backups are turned on or off..
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'BackupPolicyDescription' value with any optional fields omitted.
mkBackupPolicyDescription
    :: BackupPolicyDescription
mkBackupPolicyDescription
  = BackupPolicyDescription'{backupPolicy = Core.Nothing}

-- | Describes the file system's backup policy, indicating whether automatic backups are turned on or off..
--
-- /Note:/ Consider using 'backupPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpdBackupPolicy :: Lens.Lens' BackupPolicyDescription (Core.Maybe Types.BackupPolicy)
bpdBackupPolicy = Lens.field @"backupPolicy"
{-# INLINEABLE bpdBackupPolicy #-}
{-# DEPRECATED backupPolicy "Use generic-lens or generic-optics with 'backupPolicy' instead"  #-}

instance Core.FromJSON BackupPolicyDescription where
        parseJSON
          = Core.withObject "BackupPolicyDescription" Core.$
              \ x ->
                BackupPolicyDescription' Core.<$> (x Core..:? "BackupPolicy")

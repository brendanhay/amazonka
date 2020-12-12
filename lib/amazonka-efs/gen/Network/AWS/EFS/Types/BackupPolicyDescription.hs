{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.Types.BackupPolicyDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EFS.Types.BackupPolicyDescription
  ( BackupPolicyDescription (..),

    -- * Smart constructor
    mkBackupPolicyDescription,

    -- * Lenses
    bpdBackupPolicy,
  )
where

import Network.AWS.EFS.Types.BackupPolicy
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | /See:/ 'mkBackupPolicyDescription' smart constructor.
newtype BackupPolicyDescription = BackupPolicyDescription'
  { backupPolicy ::
      Lude.Maybe BackupPolicy
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BackupPolicyDescription' with the minimum fields required to make a request.
--
-- * 'backupPolicy' - Describes the file system's backup policy, indicating whether automatic backups are turned on or off..
mkBackupPolicyDescription ::
  BackupPolicyDescription
mkBackupPolicyDescription =
  BackupPolicyDescription' {backupPolicy = Lude.Nothing}

-- | Describes the file system's backup policy, indicating whether automatic backups are turned on or off..
--
-- /Note:/ Consider using 'backupPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpdBackupPolicy :: Lens.Lens' BackupPolicyDescription (Lude.Maybe BackupPolicy)
bpdBackupPolicy = Lens.lens (backupPolicy :: BackupPolicyDescription -> Lude.Maybe BackupPolicy) (\s a -> s {backupPolicy = a} :: BackupPolicyDescription)
{-# DEPRECATED bpdBackupPolicy "Use generic-lens or generic-optics with 'backupPolicy' instead." #-}

instance Lude.FromJSON BackupPolicyDescription where
  parseJSON =
    Lude.withObject
      "BackupPolicyDescription"
      ( \x ->
          BackupPolicyDescription' Lude.<$> (x Lude..:? "BackupPolicy")
      )

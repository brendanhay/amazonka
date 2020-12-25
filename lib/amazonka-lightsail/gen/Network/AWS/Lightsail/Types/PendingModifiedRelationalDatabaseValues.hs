{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.PendingModifiedRelationalDatabaseValues
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.PendingModifiedRelationalDatabaseValues
  ( PendingModifiedRelationalDatabaseValues (..),

    -- * Smart constructor
    mkPendingModifiedRelationalDatabaseValues,

    -- * Lenses
    pmrdvBackupRetentionEnabled,
    pmrdvEngineVersion,
    pmrdvMasterUserPassword,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types.String as Types
import qualified Network.AWS.Prelude as Core

-- | Describes a pending database value modification.
--
-- /See:/ 'mkPendingModifiedRelationalDatabaseValues' smart constructor.
data PendingModifiedRelationalDatabaseValues = PendingModifiedRelationalDatabaseValues'
  { -- | A Boolean value indicating whether automated backup retention is enabled.
    backupRetentionEnabled :: Core.Maybe Core.Bool,
    -- | The database engine version.
    engineVersion :: Core.Maybe Types.String,
    -- | The password for the master user of the database.
    masterUserPassword :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PendingModifiedRelationalDatabaseValues' value with any optional fields omitted.
mkPendingModifiedRelationalDatabaseValues ::
  PendingModifiedRelationalDatabaseValues
mkPendingModifiedRelationalDatabaseValues =
  PendingModifiedRelationalDatabaseValues'
    { backupRetentionEnabled =
        Core.Nothing,
      engineVersion = Core.Nothing,
      masterUserPassword = Core.Nothing
    }

-- | A Boolean value indicating whether automated backup retention is enabled.
--
-- /Note:/ Consider using 'backupRetentionEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmrdvBackupRetentionEnabled :: Lens.Lens' PendingModifiedRelationalDatabaseValues (Core.Maybe Core.Bool)
pmrdvBackupRetentionEnabled = Lens.field @"backupRetentionEnabled"
{-# DEPRECATED pmrdvBackupRetentionEnabled "Use generic-lens or generic-optics with 'backupRetentionEnabled' instead." #-}

-- | The database engine version.
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmrdvEngineVersion :: Lens.Lens' PendingModifiedRelationalDatabaseValues (Core.Maybe Types.String)
pmrdvEngineVersion = Lens.field @"engineVersion"
{-# DEPRECATED pmrdvEngineVersion "Use generic-lens or generic-optics with 'engineVersion' instead." #-}

-- | The password for the master user of the database.
--
-- /Note:/ Consider using 'masterUserPassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmrdvMasterUserPassword :: Lens.Lens' PendingModifiedRelationalDatabaseValues (Core.Maybe Types.String)
pmrdvMasterUserPassword = Lens.field @"masterUserPassword"
{-# DEPRECATED pmrdvMasterUserPassword "Use generic-lens or generic-optics with 'masterUserPassword' instead." #-}

instance Core.FromJSON PendingModifiedRelationalDatabaseValues where
  parseJSON =
    Core.withObject "PendingModifiedRelationalDatabaseValues" Core.$
      \x ->
        PendingModifiedRelationalDatabaseValues'
          Core.<$> (x Core..:? "backupRetentionEnabled")
          Core.<*> (x Core..:? "engineVersion")
          Core.<*> (x Core..:? "masterUserPassword")

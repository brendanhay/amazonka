{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.PendingModifiedRelationalDatabaseValues
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.PendingModifiedRelationalDatabaseValues
  ( PendingModifiedRelationalDatabaseValues (..)
  -- * Smart constructor
  , mkPendingModifiedRelationalDatabaseValues
  -- * Lenses
  , pmrdvBackupRetentionEnabled
  , pmrdvEngineVersion
  , pmrdvMasterUserPassword
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a pending database value modification.
--
-- /See:/ 'mkPendingModifiedRelationalDatabaseValues' smart constructor.
data PendingModifiedRelationalDatabaseValues = PendingModifiedRelationalDatabaseValues'
  { backupRetentionEnabled :: Core.Maybe Core.Bool
    -- ^ A Boolean value indicating whether automated backup retention is enabled.
  , engineVersion :: Core.Maybe Core.Text
    -- ^ The database engine version.
  , masterUserPassword :: Core.Maybe Core.Text
    -- ^ The password for the master user of the database.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PendingModifiedRelationalDatabaseValues' value with any optional fields omitted.
mkPendingModifiedRelationalDatabaseValues
    :: PendingModifiedRelationalDatabaseValues
mkPendingModifiedRelationalDatabaseValues
  = PendingModifiedRelationalDatabaseValues'{backupRetentionEnabled =
                                               Core.Nothing,
                                             engineVersion = Core.Nothing,
                                             masterUserPassword = Core.Nothing}

-- | A Boolean value indicating whether automated backup retention is enabled.
--
-- /Note:/ Consider using 'backupRetentionEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmrdvBackupRetentionEnabled :: Lens.Lens' PendingModifiedRelationalDatabaseValues (Core.Maybe Core.Bool)
pmrdvBackupRetentionEnabled = Lens.field @"backupRetentionEnabled"
{-# INLINEABLE pmrdvBackupRetentionEnabled #-}
{-# DEPRECATED backupRetentionEnabled "Use generic-lens or generic-optics with 'backupRetentionEnabled' instead"  #-}

-- | The database engine version.
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmrdvEngineVersion :: Lens.Lens' PendingModifiedRelationalDatabaseValues (Core.Maybe Core.Text)
pmrdvEngineVersion = Lens.field @"engineVersion"
{-# INLINEABLE pmrdvEngineVersion #-}
{-# DEPRECATED engineVersion "Use generic-lens or generic-optics with 'engineVersion' instead"  #-}

-- | The password for the master user of the database.
--
-- /Note:/ Consider using 'masterUserPassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmrdvMasterUserPassword :: Lens.Lens' PendingModifiedRelationalDatabaseValues (Core.Maybe Core.Text)
pmrdvMasterUserPassword = Lens.field @"masterUserPassword"
{-# INLINEABLE pmrdvMasterUserPassword #-}
{-# DEPRECATED masterUserPassword "Use generic-lens or generic-optics with 'masterUserPassword' instead"  #-}

instance Core.FromJSON PendingModifiedRelationalDatabaseValues
         where
        parseJSON
          = Core.withObject "PendingModifiedRelationalDatabaseValues" Core.$
              \ x ->
                PendingModifiedRelationalDatabaseValues' Core.<$>
                  (x Core..:? "backupRetentionEnabled") Core.<*>
                    x Core..:? "engineVersion"
                    Core.<*> x Core..:? "masterUserPassword"

{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.CatalogImportStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.CatalogImportStatus
  ( CatalogImportStatus (..)
  -- * Smart constructor
  , mkCatalogImportStatus
  -- * Lenses
  , cisImportCompleted
  , cisImportTime
  , cisImportedBy
  ) where

import qualified Network.AWS.Glue.Types.NameString as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A structure containing migration status information.
--
-- /See:/ 'mkCatalogImportStatus' smart constructor.
data CatalogImportStatus = CatalogImportStatus'
  { importCompleted :: Core.Maybe Core.Bool
    -- ^ @True@ if the migration has completed, or @False@ otherwise.
  , importTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The time that the migration was started.
  , importedBy :: Core.Maybe Types.NameString
    -- ^ The name of the person who initiated the migration.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CatalogImportStatus' value with any optional fields omitted.
mkCatalogImportStatus
    :: CatalogImportStatus
mkCatalogImportStatus
  = CatalogImportStatus'{importCompleted = Core.Nothing,
                         importTime = Core.Nothing, importedBy = Core.Nothing}

-- | @True@ if the migration has completed, or @False@ otherwise.
--
-- /Note:/ Consider using 'importCompleted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cisImportCompleted :: Lens.Lens' CatalogImportStatus (Core.Maybe Core.Bool)
cisImportCompleted = Lens.field @"importCompleted"
{-# INLINEABLE cisImportCompleted #-}
{-# DEPRECATED importCompleted "Use generic-lens or generic-optics with 'importCompleted' instead"  #-}

-- | The time that the migration was started.
--
-- /Note:/ Consider using 'importTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cisImportTime :: Lens.Lens' CatalogImportStatus (Core.Maybe Core.NominalDiffTime)
cisImportTime = Lens.field @"importTime"
{-# INLINEABLE cisImportTime #-}
{-# DEPRECATED importTime "Use generic-lens or generic-optics with 'importTime' instead"  #-}

-- | The name of the person who initiated the migration.
--
-- /Note:/ Consider using 'importedBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cisImportedBy :: Lens.Lens' CatalogImportStatus (Core.Maybe Types.NameString)
cisImportedBy = Lens.field @"importedBy"
{-# INLINEABLE cisImportedBy #-}
{-# DEPRECATED importedBy "Use generic-lens or generic-optics with 'importedBy' instead"  #-}

instance Core.FromJSON CatalogImportStatus where
        parseJSON
          = Core.withObject "CatalogImportStatus" Core.$
              \ x ->
                CatalogImportStatus' Core.<$>
                  (x Core..:? "ImportCompleted") Core.<*> x Core..:? "ImportTime"
                    Core.<*> x Core..:? "ImportedBy"

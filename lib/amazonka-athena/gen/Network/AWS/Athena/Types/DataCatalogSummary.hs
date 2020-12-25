{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.Types.DataCatalogSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Athena.Types.DataCatalogSummary
  ( DataCatalogSummary (..),

    -- * Smart constructor
    mkDataCatalogSummary,

    -- * Lenses
    dcsCatalogName,
    dcsType,
  )
where

import qualified Network.AWS.Athena.Types.CatalogName as Types
import qualified Network.AWS.Athena.Types.DataCatalogType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The summary information for the data catalog, which includes its name and type.
--
-- /See:/ 'mkDataCatalogSummary' smart constructor.
data DataCatalogSummary = DataCatalogSummary'
  { -- | The name of the data catalog.
    catalogName :: Core.Maybe Types.CatalogName,
    -- | The data catalog type.
    type' :: Core.Maybe Types.DataCatalogType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DataCatalogSummary' value with any optional fields omitted.
mkDataCatalogSummary ::
  DataCatalogSummary
mkDataCatalogSummary =
  DataCatalogSummary'
    { catalogName = Core.Nothing,
      type' = Core.Nothing
    }

-- | The name of the data catalog.
--
-- /Note:/ Consider using 'catalogName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsCatalogName :: Lens.Lens' DataCatalogSummary (Core.Maybe Types.CatalogName)
dcsCatalogName = Lens.field @"catalogName"
{-# DEPRECATED dcsCatalogName "Use generic-lens or generic-optics with 'catalogName' instead." #-}

-- | The data catalog type.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsType :: Lens.Lens' DataCatalogSummary (Core.Maybe Types.DataCatalogType)
dcsType = Lens.field @"type'"
{-# DEPRECATED dcsType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Core.FromJSON DataCatalogSummary where
  parseJSON =
    Core.withObject "DataCatalogSummary" Core.$
      \x ->
        DataCatalogSummary'
          Core.<$> (x Core..:? "CatalogName") Core.<*> (x Core..:? "Type")

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

import Network.AWS.Athena.Types.DataCatalogType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The summary information for the data catalog, which includes its name and type.
--
-- /See:/ 'mkDataCatalogSummary' smart constructor.
data DataCatalogSummary = DataCatalogSummary'
  { -- | The name of the data catalog.
    catalogName :: Lude.Maybe Lude.Text,
    -- | The data catalog type.
    type' :: Lude.Maybe DataCatalogType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DataCatalogSummary' with the minimum fields required to make a request.
--
-- * 'catalogName' - The name of the data catalog.
-- * 'type'' - The data catalog type.
mkDataCatalogSummary ::
  DataCatalogSummary
mkDataCatalogSummary =
  DataCatalogSummary'
    { catalogName = Lude.Nothing,
      type' = Lude.Nothing
    }

-- | The name of the data catalog.
--
-- /Note:/ Consider using 'catalogName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsCatalogName :: Lens.Lens' DataCatalogSummary (Lude.Maybe Lude.Text)
dcsCatalogName = Lens.lens (catalogName :: DataCatalogSummary -> Lude.Maybe Lude.Text) (\s a -> s {catalogName = a} :: DataCatalogSummary)
{-# DEPRECATED dcsCatalogName "Use generic-lens or generic-optics with 'catalogName' instead." #-}

-- | The data catalog type.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsType :: Lens.Lens' DataCatalogSummary (Lude.Maybe DataCatalogType)
dcsType = Lens.lens (type' :: DataCatalogSummary -> Lude.Maybe DataCatalogType) (\s a -> s {type' = a} :: DataCatalogSummary)
{-# DEPRECATED dcsType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromJSON DataCatalogSummary where
  parseJSON =
    Lude.withObject
      "DataCatalogSummary"
      ( \x ->
          DataCatalogSummary'
            Lude.<$> (x Lude..:? "CatalogName") Lude.<*> (x Lude..:? "Type")
      )

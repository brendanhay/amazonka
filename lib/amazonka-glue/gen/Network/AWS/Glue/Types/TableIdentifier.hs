-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.TableIdentifier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.TableIdentifier
  ( TableIdentifier (..),

    -- * Smart constructor
    mkTableIdentifier,

    -- * Lenses
    tiCatalogId,
    tiName,
    tiDatabaseName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A structure that describes a target table for resource linking.
--
-- /See:/ 'mkTableIdentifier' smart constructor.
data TableIdentifier = TableIdentifier'
  { catalogId ::
      Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
    databaseName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TableIdentifier' with the minimum fields required to make a request.
--
-- * 'catalogId' - The ID of the Data Catalog in which the table resides.
-- * 'databaseName' - The name of the catalog database that contains the target table.
-- * 'name' - The name of the target table.
mkTableIdentifier ::
  TableIdentifier
mkTableIdentifier =
  TableIdentifier'
    { catalogId = Lude.Nothing,
      name = Lude.Nothing,
      databaseName = Lude.Nothing
    }

-- | The ID of the Data Catalog in which the table resides.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiCatalogId :: Lens.Lens' TableIdentifier (Lude.Maybe Lude.Text)
tiCatalogId = Lens.lens (catalogId :: TableIdentifier -> Lude.Maybe Lude.Text) (\s a -> s {catalogId = a} :: TableIdentifier)
{-# DEPRECATED tiCatalogId "Use generic-lens or generic-optics with 'catalogId' instead." #-}

-- | The name of the target table.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiName :: Lens.Lens' TableIdentifier (Lude.Maybe Lude.Text)
tiName = Lens.lens (name :: TableIdentifier -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: TableIdentifier)
{-# DEPRECATED tiName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The name of the catalog database that contains the target table.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiDatabaseName :: Lens.Lens' TableIdentifier (Lude.Maybe Lude.Text)
tiDatabaseName = Lens.lens (databaseName :: TableIdentifier -> Lude.Maybe Lude.Text) (\s a -> s {databaseName = a} :: TableIdentifier)
{-# DEPRECATED tiDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

instance Lude.FromJSON TableIdentifier where
  parseJSON =
    Lude.withObject
      "TableIdentifier"
      ( \x ->
          TableIdentifier'
            Lude.<$> (x Lude..:? "CatalogId")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "DatabaseName")
      )

instance Lude.ToJSON TableIdentifier where
  toJSON TableIdentifier' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("CatalogId" Lude..=) Lude.<$> catalogId,
            ("Name" Lude..=) Lude.<$> name,
            ("DatabaseName" Lude..=) Lude.<$> databaseName
          ]
      )

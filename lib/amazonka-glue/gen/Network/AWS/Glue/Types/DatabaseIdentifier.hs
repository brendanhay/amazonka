{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.DatabaseIdentifier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.DatabaseIdentifier
  ( DatabaseIdentifier (..),

    -- * Smart constructor
    mkDatabaseIdentifier,

    -- * Lenses
    diCatalogId,
    diDatabaseName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A structure that describes a target database for resource linking.
--
-- /See:/ 'mkDatabaseIdentifier' smart constructor.
data DatabaseIdentifier = DatabaseIdentifier'
  { -- | The ID of the Data Catalog in which the database resides.
    catalogId :: Lude.Maybe Lude.Text,
    -- | The name of the catalog database.
    databaseName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DatabaseIdentifier' with the minimum fields required to make a request.
--
-- * 'catalogId' - The ID of the Data Catalog in which the database resides.
-- * 'databaseName' - The name of the catalog database.
mkDatabaseIdentifier ::
  DatabaseIdentifier
mkDatabaseIdentifier =
  DatabaseIdentifier'
    { catalogId = Lude.Nothing,
      databaseName = Lude.Nothing
    }

-- | The ID of the Data Catalog in which the database resides.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diCatalogId :: Lens.Lens' DatabaseIdentifier (Lude.Maybe Lude.Text)
diCatalogId = Lens.lens (catalogId :: DatabaseIdentifier -> Lude.Maybe Lude.Text) (\s a -> s {catalogId = a} :: DatabaseIdentifier)
{-# DEPRECATED diCatalogId "Use generic-lens or generic-optics with 'catalogId' instead." #-}

-- | The name of the catalog database.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diDatabaseName :: Lens.Lens' DatabaseIdentifier (Lude.Maybe Lude.Text)
diDatabaseName = Lens.lens (databaseName :: DatabaseIdentifier -> Lude.Maybe Lude.Text) (\s a -> s {databaseName = a} :: DatabaseIdentifier)
{-# DEPRECATED diDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

instance Lude.FromJSON DatabaseIdentifier where
  parseJSON =
    Lude.withObject
      "DatabaseIdentifier"
      ( \x ->
          DatabaseIdentifier'
            Lude.<$> (x Lude..:? "CatalogId") Lude.<*> (x Lude..:? "DatabaseName")
      )

instance Lude.ToJSON DatabaseIdentifier where
  toJSON DatabaseIdentifier' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("CatalogId" Lude..=) Lude.<$> catalogId,
            ("DatabaseName" Lude..=) Lude.<$> databaseName
          ]
      )

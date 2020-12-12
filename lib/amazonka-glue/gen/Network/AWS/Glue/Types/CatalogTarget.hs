{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.CatalogTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.CatalogTarget
  ( CatalogTarget (..),

    -- * Smart constructor
    mkCatalogTarget,

    -- * Lenses
    ctDatabaseName,
    ctTables,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies an AWS Glue Data Catalog target.
--
-- /See:/ 'mkCatalogTarget' smart constructor.
data CatalogTarget = CatalogTarget'
  { databaseName :: Lude.Text,
    tables :: Lude.NonEmpty Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CatalogTarget' with the minimum fields required to make a request.
--
-- * 'databaseName' - The name of the database to be synchronized.
-- * 'tables' - A list of the tables to be synchronized.
mkCatalogTarget ::
  -- | 'databaseName'
  Lude.Text ->
  -- | 'tables'
  Lude.NonEmpty Lude.Text ->
  CatalogTarget
mkCatalogTarget pDatabaseName_ pTables_ =
  CatalogTarget' {databaseName = pDatabaseName_, tables = pTables_}

-- | The name of the database to be synchronized.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctDatabaseName :: Lens.Lens' CatalogTarget Lude.Text
ctDatabaseName = Lens.lens (databaseName :: CatalogTarget -> Lude.Text) (\s a -> s {databaseName = a} :: CatalogTarget)
{-# DEPRECATED ctDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | A list of the tables to be synchronized.
--
-- /Note:/ Consider using 'tables' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctTables :: Lens.Lens' CatalogTarget (Lude.NonEmpty Lude.Text)
ctTables = Lens.lens (tables :: CatalogTarget -> Lude.NonEmpty Lude.Text) (\s a -> s {tables = a} :: CatalogTarget)
{-# DEPRECATED ctTables "Use generic-lens or generic-optics with 'tables' instead." #-}

instance Lude.FromJSON CatalogTarget where
  parseJSON =
    Lude.withObject
      "CatalogTarget"
      ( \x ->
          CatalogTarget'
            Lude.<$> (x Lude..: "DatabaseName") Lude.<*> (x Lude..: "Tables")
      )

instance Lude.ToJSON CatalogTarget where
  toJSON CatalogTarget' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("DatabaseName" Lude..= databaseName),
            Lude.Just ("Tables" Lude..= tables)
          ]
      )

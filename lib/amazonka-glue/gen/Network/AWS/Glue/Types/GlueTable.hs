{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.GlueTable
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.GlueTable
  ( GlueTable (..),

    -- * Smart constructor
    mkGlueTable,

    -- * Lenses
    gtCatalogId,
    gtConnectionName,
    gtDatabaseName,
    gtTableName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The database and table in the AWS Glue Data Catalog that is used for input or output data.
--
-- /See:/ 'mkGlueTable' smart constructor.
data GlueTable = GlueTable'
  { catalogId :: Lude.Maybe Lude.Text,
    connectionName :: Lude.Maybe Lude.Text,
    databaseName :: Lude.Text,
    tableName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GlueTable' with the minimum fields required to make a request.
--
-- * 'catalogId' - A unique identifier for the AWS Glue Data Catalog.
-- * 'connectionName' - The name of the connection to the AWS Glue Data Catalog.
-- * 'databaseName' - A database name in the AWS Glue Data Catalog.
-- * 'tableName' - A table name in the AWS Glue Data Catalog.
mkGlueTable ::
  -- | 'databaseName'
  Lude.Text ->
  -- | 'tableName'
  Lude.Text ->
  GlueTable
mkGlueTable pDatabaseName_ pTableName_ =
  GlueTable'
    { catalogId = Lude.Nothing,
      connectionName = Lude.Nothing,
      databaseName = pDatabaseName_,
      tableName = pTableName_
    }

-- | A unique identifier for the AWS Glue Data Catalog.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtCatalogId :: Lens.Lens' GlueTable (Lude.Maybe Lude.Text)
gtCatalogId = Lens.lens (catalogId :: GlueTable -> Lude.Maybe Lude.Text) (\s a -> s {catalogId = a} :: GlueTable)
{-# DEPRECATED gtCatalogId "Use generic-lens or generic-optics with 'catalogId' instead." #-}

-- | The name of the connection to the AWS Glue Data Catalog.
--
-- /Note:/ Consider using 'connectionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtConnectionName :: Lens.Lens' GlueTable (Lude.Maybe Lude.Text)
gtConnectionName = Lens.lens (connectionName :: GlueTable -> Lude.Maybe Lude.Text) (\s a -> s {connectionName = a} :: GlueTable)
{-# DEPRECATED gtConnectionName "Use generic-lens or generic-optics with 'connectionName' instead." #-}

-- | A database name in the AWS Glue Data Catalog.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtDatabaseName :: Lens.Lens' GlueTable Lude.Text
gtDatabaseName = Lens.lens (databaseName :: GlueTable -> Lude.Text) (\s a -> s {databaseName = a} :: GlueTable)
{-# DEPRECATED gtDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | A table name in the AWS Glue Data Catalog.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtTableName :: Lens.Lens' GlueTable Lude.Text
gtTableName = Lens.lens (tableName :: GlueTable -> Lude.Text) (\s a -> s {tableName = a} :: GlueTable)
{-# DEPRECATED gtTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

instance Lude.FromJSON GlueTable where
  parseJSON =
    Lude.withObject
      "GlueTable"
      ( \x ->
          GlueTable'
            Lude.<$> (x Lude..:? "CatalogId")
            Lude.<*> (x Lude..:? "ConnectionName")
            Lude.<*> (x Lude..: "DatabaseName")
            Lude.<*> (x Lude..: "TableName")
      )

instance Lude.ToJSON GlueTable where
  toJSON GlueTable' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("CatalogId" Lude..=) Lude.<$> catalogId,
            ("ConnectionName" Lude..=) Lude.<$> connectionName,
            Lude.Just ("DatabaseName" Lude..= databaseName),
            Lude.Just ("TableName" Lude..= tableName)
          ]
      )

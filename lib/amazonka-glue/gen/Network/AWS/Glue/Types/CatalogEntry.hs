{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.CatalogEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.CatalogEntry
  ( CatalogEntry (..),

    -- * Smart constructor
    mkCatalogEntry,

    -- * Lenses
    ceDatabaseName,
    ceTableName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies a table definition in the AWS Glue Data Catalog.
--
-- /See:/ 'mkCatalogEntry' smart constructor.
data CatalogEntry = CatalogEntry'
  { databaseName :: Lude.Text,
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

-- | Creates a value of 'CatalogEntry' with the minimum fields required to make a request.
--
-- * 'databaseName' - The database in which the table metadata resides.
-- * 'tableName' - The name of the table in question.
mkCatalogEntry ::
  -- | 'databaseName'
  Lude.Text ->
  -- | 'tableName'
  Lude.Text ->
  CatalogEntry
mkCatalogEntry pDatabaseName_ pTableName_ =
  CatalogEntry'
    { databaseName = pDatabaseName_,
      tableName = pTableName_
    }

-- | The database in which the table metadata resides.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceDatabaseName :: Lens.Lens' CatalogEntry Lude.Text
ceDatabaseName = Lens.lens (databaseName :: CatalogEntry -> Lude.Text) (\s a -> s {databaseName = a} :: CatalogEntry)
{-# DEPRECATED ceDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | The name of the table in question.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceTableName :: Lens.Lens' CatalogEntry Lude.Text
ceTableName = Lens.lens (tableName :: CatalogEntry -> Lude.Text) (\s a -> s {tableName = a} :: CatalogEntry)
{-# DEPRECATED ceTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

instance Lude.ToJSON CatalogEntry where
  toJSON CatalogEntry' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("DatabaseName" Lude..= databaseName),
            Lude.Just ("TableName" Lude..= tableName)
          ]
      )

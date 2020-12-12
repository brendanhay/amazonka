{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.TableToReload
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.TableToReload
  ( TableToReload (..),

    -- * Smart constructor
    mkTableToReload,

    -- * Lenses
    ttrSchemaName,
    ttrTableName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides the name of the schema and table to be reloaded.
--
-- /See:/ 'mkTableToReload' smart constructor.
data TableToReload = TableToReload'
  { schemaName :: Lude.Text,
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

-- | Creates a value of 'TableToReload' with the minimum fields required to make a request.
--
-- * 'schemaName' - The schema name of the table to be reloaded.
-- * 'tableName' - The table name of the table to be reloaded.
mkTableToReload ::
  -- | 'schemaName'
  Lude.Text ->
  -- | 'tableName'
  Lude.Text ->
  TableToReload
mkTableToReload pSchemaName_ pTableName_ =
  TableToReload'
    { schemaName = pSchemaName_,
      tableName = pTableName_
    }

-- | The schema name of the table to be reloaded.
--
-- /Note:/ Consider using 'schemaName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttrSchemaName :: Lens.Lens' TableToReload Lude.Text
ttrSchemaName = Lens.lens (schemaName :: TableToReload -> Lude.Text) (\s a -> s {schemaName = a} :: TableToReload)
{-# DEPRECATED ttrSchemaName "Use generic-lens or generic-optics with 'schemaName' instead." #-}

-- | The table name of the table to be reloaded.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttrTableName :: Lens.Lens' TableToReload Lude.Text
ttrTableName = Lens.lens (tableName :: TableToReload -> Lude.Text) (\s a -> s {tableName = a} :: TableToReload)
{-# DEPRECATED ttrTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

instance Lude.ToJSON TableToReload where
  toJSON TableToReload' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("SchemaName" Lude..= schemaName),
            Lude.Just ("TableName" Lude..= tableName)
          ]
      )

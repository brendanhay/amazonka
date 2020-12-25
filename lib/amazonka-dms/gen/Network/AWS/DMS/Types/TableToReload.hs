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

import qualified Network.AWS.DMS.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides the name of the schema and table to be reloaded.
--
-- /See:/ 'mkTableToReload' smart constructor.
data TableToReload = TableToReload'
  { -- | The schema name of the table to be reloaded.
    schemaName :: Types.String,
    -- | The table name of the table to be reloaded.
    tableName :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TableToReload' value with any optional fields omitted.
mkTableToReload ::
  -- | 'schemaName'
  Types.String ->
  -- | 'tableName'
  Types.String ->
  TableToReload
mkTableToReload schemaName tableName =
  TableToReload' {schemaName, tableName}

-- | The schema name of the table to be reloaded.
--
-- /Note:/ Consider using 'schemaName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttrSchemaName :: Lens.Lens' TableToReload Types.String
ttrSchemaName = Lens.field @"schemaName"
{-# DEPRECATED ttrSchemaName "Use generic-lens or generic-optics with 'schemaName' instead." #-}

-- | The table name of the table to be reloaded.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttrTableName :: Lens.Lens' TableToReload Types.String
ttrTableName = Lens.field @"tableName"
{-# DEPRECATED ttrTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

instance Core.FromJSON TableToReload where
  toJSON TableToReload {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("SchemaName" Core..= schemaName),
            Core.Just ("TableName" Core..= tableName)
          ]
      )

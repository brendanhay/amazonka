{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.TableToReload
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DMS.Types.TableToReload
  ( TableToReload (..)
  -- * Smart constructor
  , mkTableToReload
  -- * Lenses
  , ttrSchemaName
  , ttrTableName
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides the name of the schema and table to be reloaded.
--
-- /See:/ 'mkTableToReload' smart constructor.
data TableToReload = TableToReload'
  { schemaName :: Core.Text
    -- ^ The schema name of the table to be reloaded.
  , tableName :: Core.Text
    -- ^ The table name of the table to be reloaded.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TableToReload' value with any optional fields omitted.
mkTableToReload
    :: Core.Text -- ^ 'schemaName'
    -> Core.Text -- ^ 'tableName'
    -> TableToReload
mkTableToReload schemaName tableName
  = TableToReload'{schemaName, tableName}

-- | The schema name of the table to be reloaded.
--
-- /Note:/ Consider using 'schemaName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttrSchemaName :: Lens.Lens' TableToReload Core.Text
ttrSchemaName = Lens.field @"schemaName"
{-# INLINEABLE ttrSchemaName #-}
{-# DEPRECATED schemaName "Use generic-lens or generic-optics with 'schemaName' instead"  #-}

-- | The table name of the table to be reloaded.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttrTableName :: Lens.Lens' TableToReload Core.Text
ttrTableName = Lens.field @"tableName"
{-# INLINEABLE ttrTableName #-}
{-# DEPRECATED tableName "Use generic-lens or generic-optics with 'tableName' instead"  #-}

instance Core.FromJSON TableToReload where
        toJSON TableToReload{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("SchemaName" Core..= schemaName),
                  Core.Just ("TableName" Core..= tableName)])

{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.TableVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.TableVersion
  ( TableVersion (..)
  -- * Smart constructor
  , mkTableVersion
  -- * Lenses
  , tvTable
  , tvVersionId
  ) where

import qualified Network.AWS.Glue.Types.Table as Types
import qualified Network.AWS.Glue.Types.VersionString as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies a version of a table.
--
-- /See:/ 'mkTableVersion' smart constructor.
data TableVersion = TableVersion'
  { table :: Core.Maybe Types.Table
    -- ^ The table in question.
  , versionId :: Core.Maybe Types.VersionString
    -- ^ The ID value that identifies this table version. A @VersionId@ is a string representation of an integer. Each version is incremented by 1.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'TableVersion' value with any optional fields omitted.
mkTableVersion
    :: TableVersion
mkTableVersion
  = TableVersion'{table = Core.Nothing, versionId = Core.Nothing}

-- | The table in question.
--
-- /Note:/ Consider using 'table' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tvTable :: Lens.Lens' TableVersion (Core.Maybe Types.Table)
tvTable = Lens.field @"table"
{-# INLINEABLE tvTable #-}
{-# DEPRECATED table "Use generic-lens or generic-optics with 'table' instead"  #-}

-- | The ID value that identifies this table version. A @VersionId@ is a string representation of an integer. Each version is incremented by 1.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tvVersionId :: Lens.Lens' TableVersion (Core.Maybe Types.VersionString)
tvVersionId = Lens.field @"versionId"
{-# INLINEABLE tvVersionId #-}
{-# DEPRECATED versionId "Use generic-lens or generic-optics with 'versionId' instead"  #-}

instance Core.FromJSON TableVersion where
        parseJSON
          = Core.withObject "TableVersion" Core.$
              \ x ->
                TableVersion' Core.<$>
                  (x Core..:? "Table") Core.<*> x Core..:? "VersionId"

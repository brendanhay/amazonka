{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.MongoDBTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.MongoDBTarget
  ( MongoDBTarget (..)
  -- * Smart constructor
  , mkMongoDBTarget
  -- * Lenses
  , mdbtConnectionName
  , mdbtPath
  , mdbtScanAll
  ) where

import qualified Network.AWS.Glue.Types.ConnectionName as Types
import qualified Network.AWS.Glue.Types.Path as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies an Amazon DocumentDB or MongoDB data store to crawl.
--
-- /See:/ 'mkMongoDBTarget' smart constructor.
data MongoDBTarget = MongoDBTarget'
  { connectionName :: Core.Maybe Types.ConnectionName
    -- ^ The name of the connection to use to connect to the Amazon DocumentDB or MongoDB target.
  , path :: Core.Maybe Types.Path
    -- ^ The path of the Amazon DocumentDB or MongoDB target (database/collection).
  , scanAll :: Core.Maybe Core.Bool
    -- ^ Indicates whether to scan all the records, or to sample rows from the table. Scanning all the records can take a long time when the table is not a high throughput table.
--
-- A value of @true@ means to scan all records, while a value of @false@ means to sample the records. If no value is specified, the value defaults to @true@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MongoDBTarget' value with any optional fields omitted.
mkMongoDBTarget
    :: MongoDBTarget
mkMongoDBTarget
  = MongoDBTarget'{connectionName = Core.Nothing,
                   path = Core.Nothing, scanAll = Core.Nothing}

-- | The name of the connection to use to connect to the Amazon DocumentDB or MongoDB target.
--
-- /Note:/ Consider using 'connectionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbtConnectionName :: Lens.Lens' MongoDBTarget (Core.Maybe Types.ConnectionName)
mdbtConnectionName = Lens.field @"connectionName"
{-# INLINEABLE mdbtConnectionName #-}
{-# DEPRECATED connectionName "Use generic-lens or generic-optics with 'connectionName' instead"  #-}

-- | The path of the Amazon DocumentDB or MongoDB target (database/collection).
--
-- /Note:/ Consider using 'path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbtPath :: Lens.Lens' MongoDBTarget (Core.Maybe Types.Path)
mdbtPath = Lens.field @"path"
{-# INLINEABLE mdbtPath #-}
{-# DEPRECATED path "Use generic-lens or generic-optics with 'path' instead"  #-}

-- | Indicates whether to scan all the records, or to sample rows from the table. Scanning all the records can take a long time when the table is not a high throughput table.
--
-- A value of @true@ means to scan all records, while a value of @false@ means to sample the records. If no value is specified, the value defaults to @true@ .
--
-- /Note:/ Consider using 'scanAll' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbtScanAll :: Lens.Lens' MongoDBTarget (Core.Maybe Core.Bool)
mdbtScanAll = Lens.field @"scanAll"
{-# INLINEABLE mdbtScanAll #-}
{-# DEPRECATED scanAll "Use generic-lens or generic-optics with 'scanAll' instead"  #-}

instance Core.FromJSON MongoDBTarget where
        toJSON MongoDBTarget{..}
          = Core.object
              (Core.catMaybes
                 [("ConnectionName" Core..=) Core.<$> connectionName,
                  ("Path" Core..=) Core.<$> path,
                  ("ScanAll" Core..=) Core.<$> scanAll])

instance Core.FromJSON MongoDBTarget where
        parseJSON
          = Core.withObject "MongoDBTarget" Core.$
              \ x ->
                MongoDBTarget' Core.<$>
                  (x Core..:? "ConnectionName") Core.<*> x Core..:? "Path" Core.<*>
                    x Core..:? "ScanAll"

{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.CacheParameterGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElastiCache.Types.CacheParameterGroup
  ( CacheParameterGroup (..)
  -- * Smart constructor
  , mkCacheParameterGroup
  -- * Lenses
  , cpgARN
  , cpgCacheParameterGroupFamily
  , cpgCacheParameterGroupName
  , cpgDescription
  , cpgIsGlobal
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the output of a @CreateCacheParameterGroup@ operation.
--
-- /See:/ 'mkCacheParameterGroup' smart constructor.
data CacheParameterGroup = CacheParameterGroup'
  { arn :: Core.Maybe Core.Text
    -- ^ The ARN (Amazon Resource Name) of the cache parameter group.
  , cacheParameterGroupFamily :: Core.Maybe Core.Text
    -- ^ The name of the cache parameter group family that this cache parameter group is compatible with.
--
-- Valid values are: @memcached1.4@ | @memcached1.5@ | @memcached1.6@ | @redis2.6@ | @redis2.8@ | @redis3.2@ | @redis4.0@ | @redis5.0@ | @redis6.x@ | 
  , cacheParameterGroupName :: Core.Maybe Core.Text
    -- ^ The name of the cache parameter group.
  , description :: Core.Maybe Core.Text
    -- ^ The description for this cache parameter group.
  , isGlobal :: Core.Maybe Core.Bool
    -- ^ Indicates whether the parameter group is associated with a Global Datastore
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CacheParameterGroup' value with any optional fields omitted.
mkCacheParameterGroup
    :: CacheParameterGroup
mkCacheParameterGroup
  = CacheParameterGroup'{arn = Core.Nothing,
                         cacheParameterGroupFamily = Core.Nothing,
                         cacheParameterGroupName = Core.Nothing, description = Core.Nothing,
                         isGlobal = Core.Nothing}

-- | The ARN (Amazon Resource Name) of the cache parameter group.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpgARN :: Lens.Lens' CacheParameterGroup (Core.Maybe Core.Text)
cpgARN = Lens.field @"arn"
{-# INLINEABLE cpgARN #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The name of the cache parameter group family that this cache parameter group is compatible with.
--
-- Valid values are: @memcached1.4@ | @memcached1.5@ | @memcached1.6@ | @redis2.6@ | @redis2.8@ | @redis3.2@ | @redis4.0@ | @redis5.0@ | @redis6.x@ | 
--
-- /Note:/ Consider using 'cacheParameterGroupFamily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpgCacheParameterGroupFamily :: Lens.Lens' CacheParameterGroup (Core.Maybe Core.Text)
cpgCacheParameterGroupFamily = Lens.field @"cacheParameterGroupFamily"
{-# INLINEABLE cpgCacheParameterGroupFamily #-}
{-# DEPRECATED cacheParameterGroupFamily "Use generic-lens or generic-optics with 'cacheParameterGroupFamily' instead"  #-}

-- | The name of the cache parameter group.
--
-- /Note:/ Consider using 'cacheParameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpgCacheParameterGroupName :: Lens.Lens' CacheParameterGroup (Core.Maybe Core.Text)
cpgCacheParameterGroupName = Lens.field @"cacheParameterGroupName"
{-# INLINEABLE cpgCacheParameterGroupName #-}
{-# DEPRECATED cacheParameterGroupName "Use generic-lens or generic-optics with 'cacheParameterGroupName' instead"  #-}

-- | The description for this cache parameter group.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpgDescription :: Lens.Lens' CacheParameterGroup (Core.Maybe Core.Text)
cpgDescription = Lens.field @"description"
{-# INLINEABLE cpgDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | Indicates whether the parameter group is associated with a Global Datastore
--
-- /Note:/ Consider using 'isGlobal' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpgIsGlobal :: Lens.Lens' CacheParameterGroup (Core.Maybe Core.Bool)
cpgIsGlobal = Lens.field @"isGlobal"
{-# INLINEABLE cpgIsGlobal #-}
{-# DEPRECATED isGlobal "Use generic-lens or generic-optics with 'isGlobal' instead"  #-}

instance Core.FromXML CacheParameterGroup where
        parseXML x
          = CacheParameterGroup' Core.<$>
              (x Core..@? "ARN") Core.<*> x Core..@? "CacheParameterGroupFamily"
                Core.<*> x Core..@? "CacheParameterGroupName"
                Core.<*> x Core..@? "Description"
                Core.<*> x Core..@? "IsGlobal"

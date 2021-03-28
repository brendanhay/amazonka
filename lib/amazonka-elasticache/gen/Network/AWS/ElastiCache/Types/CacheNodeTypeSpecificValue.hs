{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.CacheNodeTypeSpecificValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElastiCache.Types.CacheNodeTypeSpecificValue
  ( CacheNodeTypeSpecificValue (..)
  -- * Smart constructor
  , mkCacheNodeTypeSpecificValue
  -- * Lenses
  , cntsvCacheNodeType
  , cntsvValue
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A value that applies only to a certain cache node type.
--
-- /See:/ 'mkCacheNodeTypeSpecificValue' smart constructor.
data CacheNodeTypeSpecificValue = CacheNodeTypeSpecificValue'
  { cacheNodeType :: Core.Maybe Core.Text
    -- ^ The cache node type for which this value applies.
  , value :: Core.Maybe Core.Text
    -- ^ The value for the cache node type.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CacheNodeTypeSpecificValue' value with any optional fields omitted.
mkCacheNodeTypeSpecificValue
    :: CacheNodeTypeSpecificValue
mkCacheNodeTypeSpecificValue
  = CacheNodeTypeSpecificValue'{cacheNodeType = Core.Nothing,
                                value = Core.Nothing}

-- | The cache node type for which this value applies.
--
-- /Note:/ Consider using 'cacheNodeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cntsvCacheNodeType :: Lens.Lens' CacheNodeTypeSpecificValue (Core.Maybe Core.Text)
cntsvCacheNodeType = Lens.field @"cacheNodeType"
{-# INLINEABLE cntsvCacheNodeType #-}
{-# DEPRECATED cacheNodeType "Use generic-lens or generic-optics with 'cacheNodeType' instead"  #-}

-- | The value for the cache node type.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cntsvValue :: Lens.Lens' CacheNodeTypeSpecificValue (Core.Maybe Core.Text)
cntsvValue = Lens.field @"value"
{-# INLINEABLE cntsvValue #-}
{-# DEPRECATED value "Use generic-lens or generic-optics with 'value' instead"  #-}

instance Core.FromXML CacheNodeTypeSpecificValue where
        parseXML x
          = CacheNodeTypeSpecificValue' Core.<$>
              (x Core..@? "CacheNodeType") Core.<*> x Core..@? "Value"

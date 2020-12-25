{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.CacheParameterGroupNameMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.CacheParameterGroupNameMessage
  ( CacheParameterGroupNameMessage (..),

    -- * Smart constructor
    mkCacheParameterGroupNameMessage,

    -- * Lenses
    cpgnmCacheParameterGroupName,
  )
where

import qualified Network.AWS.ElastiCache.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the output of one of the following operations:
--
--
--     * @ModifyCacheParameterGroup@
--
--
--     * @ResetCacheParameterGroup@
--
--
--
-- /See:/ 'mkCacheParameterGroupNameMessage' smart constructor.
newtype CacheParameterGroupNameMessage = CacheParameterGroupNameMessage'
  { -- | The name of the cache parameter group.
    cacheParameterGroupName :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CacheParameterGroupNameMessage' value with any optional fields omitted.
mkCacheParameterGroupNameMessage ::
  CacheParameterGroupNameMessage
mkCacheParameterGroupNameMessage =
  CacheParameterGroupNameMessage'
    { cacheParameterGroupName =
        Core.Nothing
    }

-- | The name of the cache parameter group.
--
-- /Note:/ Consider using 'cacheParameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpgnmCacheParameterGroupName :: Lens.Lens' CacheParameterGroupNameMessage (Core.Maybe Types.String)
cpgnmCacheParameterGroupName = Lens.field @"cacheParameterGroupName"
{-# DEPRECATED cpgnmCacheParameterGroupName "Use generic-lens or generic-optics with 'cacheParameterGroupName' instead." #-}

instance Core.FromXML CacheParameterGroupNameMessage where
  parseXML x =
    CacheParameterGroupNameMessage'
      Core.<$> (x Core..@? "CacheParameterGroupName")

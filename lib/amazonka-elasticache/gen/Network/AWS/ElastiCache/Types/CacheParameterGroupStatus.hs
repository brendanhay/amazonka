{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.CacheParameterGroupStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.CacheParameterGroupStatus
  ( CacheParameterGroupStatus (..),

    -- * Smart constructor
    mkCacheParameterGroupStatus,

    -- * Lenses
    cpgsCacheNodeIdsToReboot,
    cpgsCacheParameterGroupName,
    cpgsParameterApplyStatus,
  )
where

import qualified Network.AWS.ElastiCache.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Status of the cache parameter group.
--
-- /See:/ 'mkCacheParameterGroupStatus' smart constructor.
data CacheParameterGroupStatus = CacheParameterGroupStatus'
  { -- | A list of the cache node IDs which need to be rebooted for parameter changes to be applied. A node ID is a numeric identifier (0001, 0002, etc.).
    cacheNodeIdsToReboot :: Core.Maybe [Types.String],
    -- | The name of the cache parameter group.
    cacheParameterGroupName :: Core.Maybe Types.String,
    -- | The status of parameter updates.
    parameterApplyStatus :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CacheParameterGroupStatus' value with any optional fields omitted.
mkCacheParameterGroupStatus ::
  CacheParameterGroupStatus
mkCacheParameterGroupStatus =
  CacheParameterGroupStatus'
    { cacheNodeIdsToReboot = Core.Nothing,
      cacheParameterGroupName = Core.Nothing,
      parameterApplyStatus = Core.Nothing
    }

-- | A list of the cache node IDs which need to be rebooted for parameter changes to be applied. A node ID is a numeric identifier (0001, 0002, etc.).
--
-- /Note:/ Consider using 'cacheNodeIdsToReboot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpgsCacheNodeIdsToReboot :: Lens.Lens' CacheParameterGroupStatus (Core.Maybe [Types.String])
cpgsCacheNodeIdsToReboot = Lens.field @"cacheNodeIdsToReboot"
{-# DEPRECATED cpgsCacheNodeIdsToReboot "Use generic-lens or generic-optics with 'cacheNodeIdsToReboot' instead." #-}

-- | The name of the cache parameter group.
--
-- /Note:/ Consider using 'cacheParameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpgsCacheParameterGroupName :: Lens.Lens' CacheParameterGroupStatus (Core.Maybe Types.String)
cpgsCacheParameterGroupName = Lens.field @"cacheParameterGroupName"
{-# DEPRECATED cpgsCacheParameterGroupName "Use generic-lens or generic-optics with 'cacheParameterGroupName' instead." #-}

-- | The status of parameter updates.
--
-- /Note:/ Consider using 'parameterApplyStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpgsParameterApplyStatus :: Lens.Lens' CacheParameterGroupStatus (Core.Maybe Types.String)
cpgsParameterApplyStatus = Lens.field @"parameterApplyStatus"
{-# DEPRECATED cpgsParameterApplyStatus "Use generic-lens or generic-optics with 'parameterApplyStatus' instead." #-}

instance Core.FromXML CacheParameterGroupStatus where
  parseXML x =
    CacheParameterGroupStatus'
      Core.<$> ( x Core..@? "CacheNodeIdsToReboot"
                   Core..<@> Core.parseXMLList "CacheNodeId"
               )
      Core.<*> (x Core..@? "CacheParameterGroupName")
      Core.<*> (x Core..@? "ParameterApplyStatus")

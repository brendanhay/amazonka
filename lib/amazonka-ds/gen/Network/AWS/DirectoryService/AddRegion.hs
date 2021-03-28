{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.AddRegion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds two domain controllers in the specified Region for the specified directory.
module Network.AWS.DirectoryService.AddRegion
    (
    -- * Creating a request
      AddRegion (..)
    , mkAddRegion
    -- ** Request lenses
    , arDirectoryId
    , arRegionName
    , arVPCSettings

    -- * Destructuring the response
    , AddRegionResponse (..)
    , mkAddRegionResponse
    -- ** Response lenses
    , arrrsResponseStatus
    ) where

import qualified Network.AWS.DirectoryService.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAddRegion' smart constructor.
data AddRegion = AddRegion'
  { directoryId :: Types.DirectoryId
    -- ^ The identifier of the directory to which you want to add Region replication.
  , regionName :: Types.RegionName
    -- ^ The name of the Region where you want to add domain controllers for replication. For example, @us-east-1@ .
  , vPCSettings :: Types.DirectoryVpcSettings
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AddRegion' value with any optional fields omitted.
mkAddRegion
    :: Types.DirectoryId -- ^ 'directoryId'
    -> Types.RegionName -- ^ 'regionName'
    -> Types.DirectoryVpcSettings -- ^ 'vPCSettings'
    -> AddRegion
mkAddRegion directoryId regionName vPCSettings
  = AddRegion'{directoryId, regionName, vPCSettings}

-- | The identifier of the directory to which you want to add Region replication.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arDirectoryId :: Lens.Lens' AddRegion Types.DirectoryId
arDirectoryId = Lens.field @"directoryId"
{-# INLINEABLE arDirectoryId #-}
{-# DEPRECATED directoryId "Use generic-lens or generic-optics with 'directoryId' instead"  #-}

-- | The name of the Region where you want to add domain controllers for replication. For example, @us-east-1@ .
--
-- /Note:/ Consider using 'regionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arRegionName :: Lens.Lens' AddRegion Types.RegionName
arRegionName = Lens.field @"regionName"
{-# INLINEABLE arRegionName #-}
{-# DEPRECATED regionName "Use generic-lens or generic-optics with 'regionName' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'vPCSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arVPCSettings :: Lens.Lens' AddRegion Types.DirectoryVpcSettings
arVPCSettings = Lens.field @"vPCSettings"
{-# INLINEABLE arVPCSettings #-}
{-# DEPRECATED vPCSettings "Use generic-lens or generic-optics with 'vPCSettings' instead"  #-}

instance Core.ToQuery AddRegion where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders AddRegion where
        toHeaders AddRegion{..}
          = Core.pure ("X-Amz-Target", "DirectoryService_20150416.AddRegion")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON AddRegion where
        toJSON AddRegion{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("DirectoryId" Core..= directoryId),
                  Core.Just ("RegionName" Core..= regionName),
                  Core.Just ("VPCSettings" Core..= vPCSettings)])

instance Core.AWSRequest AddRegion where
        type Rs AddRegion = AddRegionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 AddRegionResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkAddRegionResponse' smart constructor.
newtype AddRegionResponse = AddRegionResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AddRegionResponse' value with any optional fields omitted.
mkAddRegionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> AddRegionResponse
mkAddRegionResponse responseStatus
  = AddRegionResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arrrsResponseStatus :: Lens.Lens' AddRegionResponse Core.Int
arrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE arrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

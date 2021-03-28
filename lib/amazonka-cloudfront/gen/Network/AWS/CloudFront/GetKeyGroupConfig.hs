{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.GetKeyGroupConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a key group configuration.
--
-- To get a key group configuration, you must provide the key group’s identifier. If the key group is referenced in a distribution’s cache behavior, you can get the key group’s identifier using @ListDistributions@ or @GetDistribution@ . If the key group is not referenced in a cache behavior, you can get the identifier using @ListKeyGroups@ .
module Network.AWS.CloudFront.GetKeyGroupConfig
    (
    -- * Creating a request
      GetKeyGroupConfig (..)
    , mkGetKeyGroupConfig
    -- ** Request lenses
    , gkgcId

    -- * Destructuring the response
    , GetKeyGroupConfigResponse (..)
    , mkGetKeyGroupConfigResponse
    -- ** Response lenses
    , gkgcrrsETag
    , gkgcrrsKeyGroupConfig
    , gkgcrrsResponseStatus
    ) where

import qualified Network.AWS.CloudFront.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetKeyGroupConfig' smart constructor.
newtype GetKeyGroupConfig = GetKeyGroupConfig'
  { id :: Core.Text
    -- ^ The identifier of the key group whose configuration you are getting. To get the identifier, use @ListKeyGroups@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetKeyGroupConfig' value with any optional fields omitted.
mkGetKeyGroupConfig
    :: Core.Text -- ^ 'id'
    -> GetKeyGroupConfig
mkGetKeyGroupConfig id = GetKeyGroupConfig'{id}

-- | The identifier of the key group whose configuration you are getting. To get the identifier, use @ListKeyGroups@ .
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gkgcId :: Lens.Lens' GetKeyGroupConfig Core.Text
gkgcId = Lens.field @"id"
{-# INLINEABLE gkgcId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

instance Core.ToQuery GetKeyGroupConfig where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetKeyGroupConfig where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest GetKeyGroupConfig where
        type Rs GetKeyGroupConfig = GetKeyGroupConfigResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/2020-05-31/key-group/" Core.<> Core.toText id Core.<> "/config",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 GetKeyGroupConfigResponse' Core.<$>
                   (Core.parseHeaderMaybe "ETag" h) Core.<*> Core.parseXML x Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetKeyGroupConfigResponse' smart constructor.
data GetKeyGroupConfigResponse = GetKeyGroupConfigResponse'
  { eTag :: Core.Maybe Core.Text
    -- ^ The identifier for this version of the key group.
  , keyGroupConfig :: Core.Maybe Types.KeyGroupConfig
    -- ^ The key group configuration.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetKeyGroupConfigResponse' value with any optional fields omitted.
mkGetKeyGroupConfigResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetKeyGroupConfigResponse
mkGetKeyGroupConfigResponse responseStatus
  = GetKeyGroupConfigResponse'{eTag = Core.Nothing,
                               keyGroupConfig = Core.Nothing, responseStatus}

-- | The identifier for this version of the key group.
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gkgcrrsETag :: Lens.Lens' GetKeyGroupConfigResponse (Core.Maybe Core.Text)
gkgcrrsETag = Lens.field @"eTag"
{-# INLINEABLE gkgcrrsETag #-}
{-# DEPRECATED eTag "Use generic-lens or generic-optics with 'eTag' instead"  #-}

-- | The key group configuration.
--
-- /Note:/ Consider using 'keyGroupConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gkgcrrsKeyGroupConfig :: Lens.Lens' GetKeyGroupConfigResponse (Core.Maybe Types.KeyGroupConfig)
gkgcrrsKeyGroupConfig = Lens.field @"keyGroupConfig"
{-# INLINEABLE gkgcrrsKeyGroupConfig #-}
{-# DEPRECATED keyGroupConfig "Use generic-lens or generic-optics with 'keyGroupConfig' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gkgcrrsResponseStatus :: Lens.Lens' GetKeyGroupConfigResponse Core.Int
gkgcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gkgcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

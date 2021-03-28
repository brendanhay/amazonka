{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.GetKeyGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a key group, including the date and time when the key group was last modified.
--
-- To get a key group, you must provide the key group’s identifier. If the key group is referenced in a distribution’s cache behavior, you can get the key group’s identifier using @ListDistributions@ or @GetDistribution@ . If the key group is not referenced in a cache behavior, you can get the identifier using @ListKeyGroups@ .
module Network.AWS.CloudFront.GetKeyGroup
    (
    -- * Creating a request
      GetKeyGroup (..)
    , mkGetKeyGroup
    -- ** Request lenses
    , gkgId

    -- * Destructuring the response
    , GetKeyGroupResponse (..)
    , mkGetKeyGroupResponse
    -- ** Response lenses
    , gkgrrsETag
    , gkgrrsKeyGroup
    , gkgrrsResponseStatus
    ) where

import qualified Network.AWS.CloudFront.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetKeyGroup' smart constructor.
newtype GetKeyGroup = GetKeyGroup'
  { id :: Core.Text
    -- ^ The identifier of the key group that you are getting. To get the identifier, use @ListKeyGroups@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetKeyGroup' value with any optional fields omitted.
mkGetKeyGroup
    :: Core.Text -- ^ 'id'
    -> GetKeyGroup
mkGetKeyGroup id = GetKeyGroup'{id}

-- | The identifier of the key group that you are getting. To get the identifier, use @ListKeyGroups@ .
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gkgId :: Lens.Lens' GetKeyGroup Core.Text
gkgId = Lens.field @"id"
{-# INLINEABLE gkgId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

instance Core.ToQuery GetKeyGroup where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetKeyGroup where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest GetKeyGroup where
        type Rs GetKeyGroup = GetKeyGroupResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath = "/2020-05-31/key-group/" Core.<> Core.toText id,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 GetKeyGroupResponse' Core.<$>
                   (Core.parseHeaderMaybe "ETag" h) Core.<*> Core.parseXML x Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetKeyGroupResponse' smart constructor.
data GetKeyGroupResponse = GetKeyGroupResponse'
  { eTag :: Core.Maybe Core.Text
    -- ^ The identifier for this version of the key group.
  , keyGroup :: Core.Maybe Types.KeyGroup
    -- ^ The key group.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetKeyGroupResponse' value with any optional fields omitted.
mkGetKeyGroupResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetKeyGroupResponse
mkGetKeyGroupResponse responseStatus
  = GetKeyGroupResponse'{eTag = Core.Nothing,
                         keyGroup = Core.Nothing, responseStatus}

-- | The identifier for this version of the key group.
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gkgrrsETag :: Lens.Lens' GetKeyGroupResponse (Core.Maybe Core.Text)
gkgrrsETag = Lens.field @"eTag"
{-# INLINEABLE gkgrrsETag #-}
{-# DEPRECATED eTag "Use generic-lens or generic-optics with 'eTag' instead"  #-}

-- | The key group.
--
-- /Note:/ Consider using 'keyGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gkgrrsKeyGroup :: Lens.Lens' GetKeyGroupResponse (Core.Maybe Types.KeyGroup)
gkgrrsKeyGroup = Lens.field @"keyGroup"
{-# INLINEABLE gkgrrsKeyGroup #-}
{-# DEPRECATED keyGroup "Use generic-lens or generic-optics with 'keyGroup' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gkgrrsResponseStatus :: Lens.Lens' GetKeyGroupResponse Core.Int
gkgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gkgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

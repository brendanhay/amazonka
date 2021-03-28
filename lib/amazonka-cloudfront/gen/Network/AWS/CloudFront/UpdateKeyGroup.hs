{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.UpdateKeyGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a key group.
--
-- When you update a key group, all the fields are updated with the values provided in the request. You cannot update some fields independent of others. To update a key group:
--
--     * Get the current key group with @GetKeyGroup@ or @GetKeyGroupConfig@ .
--
--
--     * Locally modify the fields in the key group that you want to update. For example, add or remove public key IDs.
--
--
--     * Call @UpdateKeyGroup@ with the entire key group object, including the fields that you modified and those that you didn’t.
--
--
module Network.AWS.CloudFront.UpdateKeyGroup
    (
    -- * Creating a request
      UpdateKeyGroup (..)
    , mkUpdateKeyGroup
    -- ** Request lenses
    , ukgKeyGroupConfig
    , ukgId
    , ukgIfMatch

    -- * Destructuring the response
    , UpdateKeyGroupResponse (..)
    , mkUpdateKeyGroupResponse
    -- ** Response lenses
    , ukgrrsETag
    , ukgrrsKeyGroup
    , ukgrrsResponseStatus
    ) where

import qualified Network.AWS.CloudFront.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateKeyGroup' smart constructor.
data UpdateKeyGroup = UpdateKeyGroup'
  { keyGroupConfig :: Types.KeyGroupConfig
    -- ^ The key group configuration.
  , id :: Core.Text
    -- ^ The identifier of the key group that you are updating.
  , ifMatch :: Core.Maybe Core.Text
    -- ^ The version of the key group that you are updating. The version is the key group’s @ETag@ value.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateKeyGroup' value with any optional fields omitted.
mkUpdateKeyGroup
    :: Types.KeyGroupConfig -- ^ 'keyGroupConfig'
    -> Core.Text -- ^ 'id'
    -> UpdateKeyGroup
mkUpdateKeyGroup keyGroupConfig id
  = UpdateKeyGroup'{keyGroupConfig, id, ifMatch = Core.Nothing}

-- | The key group configuration.
--
-- /Note:/ Consider using 'keyGroupConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ukgKeyGroupConfig :: Lens.Lens' UpdateKeyGroup Types.KeyGroupConfig
ukgKeyGroupConfig = Lens.field @"keyGroupConfig"
{-# INLINEABLE ukgKeyGroupConfig #-}
{-# DEPRECATED keyGroupConfig "Use generic-lens or generic-optics with 'keyGroupConfig' instead"  #-}

-- | The identifier of the key group that you are updating.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ukgId :: Lens.Lens' UpdateKeyGroup Core.Text
ukgId = Lens.field @"id"
{-# INLINEABLE ukgId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The version of the key group that you are updating. The version is the key group’s @ETag@ value.
--
-- /Note:/ Consider using 'ifMatch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ukgIfMatch :: Lens.Lens' UpdateKeyGroup (Core.Maybe Core.Text)
ukgIfMatch = Lens.field @"ifMatch"
{-# INLINEABLE ukgIfMatch #-}
{-# DEPRECATED ifMatch "Use generic-lens or generic-optics with 'ifMatch' instead"  #-}

instance Core.ToQuery UpdateKeyGroup where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateKeyGroup where
        toHeaders UpdateKeyGroup{..} = Core.toHeaders "If-Match" ifMatch

instance Core.AWSRequest UpdateKeyGroup where
        type Rs UpdateKeyGroup = UpdateKeyGroupResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath = "/2020-05-31/key-group/" Core.<> Core.toText id,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toXMLBody (Core.toXMLDocument x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 UpdateKeyGroupResponse' Core.<$>
                   (Core.parseHeaderMaybe "ETag" h) Core.<*> Core.parseXML x Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateKeyGroupResponse' smart constructor.
data UpdateKeyGroupResponse = UpdateKeyGroupResponse'
  { eTag :: Core.Maybe Core.Text
    -- ^ The identifier for this version of the key group.
  , keyGroup :: Core.Maybe Types.KeyGroup
    -- ^ The key group that was just updated.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'UpdateKeyGroupResponse' value with any optional fields omitted.
mkUpdateKeyGroupResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateKeyGroupResponse
mkUpdateKeyGroupResponse responseStatus
  = UpdateKeyGroupResponse'{eTag = Core.Nothing,
                            keyGroup = Core.Nothing, responseStatus}

-- | The identifier for this version of the key group.
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ukgrrsETag :: Lens.Lens' UpdateKeyGroupResponse (Core.Maybe Core.Text)
ukgrrsETag = Lens.field @"eTag"
{-# INLINEABLE ukgrrsETag #-}
{-# DEPRECATED eTag "Use generic-lens or generic-optics with 'eTag' instead"  #-}

-- | The key group that was just updated.
--
-- /Note:/ Consider using 'keyGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ukgrrsKeyGroup :: Lens.Lens' UpdateKeyGroupResponse (Core.Maybe Types.KeyGroup)
ukgrrsKeyGroup = Lens.field @"keyGroup"
{-# INLINEABLE ukgrrsKeyGroup #-}
{-# DEPRECATED keyGroup "Use generic-lens or generic-optics with 'keyGroup' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ukgrrsResponseStatus :: Lens.Lens' UpdateKeyGroupResponse Core.Int
ukgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ukgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

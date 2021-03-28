{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.ModifyClientProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the properties of the specified Amazon WorkSpaces clients.
module Network.AWS.WorkSpaces.ModifyClientProperties
    (
    -- * Creating a request
      ModifyClientProperties (..)
    , mkModifyClientProperties
    -- ** Request lenses
    , mcpResourceId
    , mcpClientProperties

    -- * Destructuring the response
    , ModifyClientPropertiesResponse (..)
    , mkModifyClientPropertiesResponse
    -- ** Response lenses
    , mcprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkSpaces.Types as Types

-- | /See:/ 'mkModifyClientProperties' smart constructor.
data ModifyClientProperties = ModifyClientProperties'
  { resourceId :: Types.ResourceId
    -- ^ The resource identifiers, in the form of directory IDs.
  , clientProperties :: Types.ClientProperties
    -- ^ Information about the Amazon WorkSpaces client.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyClientProperties' value with any optional fields omitted.
mkModifyClientProperties
    :: Types.ResourceId -- ^ 'resourceId'
    -> Types.ClientProperties -- ^ 'clientProperties'
    -> ModifyClientProperties
mkModifyClientProperties resourceId clientProperties
  = ModifyClientProperties'{resourceId, clientProperties}

-- | The resource identifiers, in the form of directory IDs.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcpResourceId :: Lens.Lens' ModifyClientProperties Types.ResourceId
mcpResourceId = Lens.field @"resourceId"
{-# INLINEABLE mcpResourceId #-}
{-# DEPRECATED resourceId "Use generic-lens or generic-optics with 'resourceId' instead"  #-}

-- | Information about the Amazon WorkSpaces client.
--
-- /Note:/ Consider using 'clientProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcpClientProperties :: Lens.Lens' ModifyClientProperties Types.ClientProperties
mcpClientProperties = Lens.field @"clientProperties"
{-# INLINEABLE mcpClientProperties #-}
{-# DEPRECATED clientProperties "Use generic-lens or generic-optics with 'clientProperties' instead"  #-}

instance Core.ToQuery ModifyClientProperties where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ModifyClientProperties where
        toHeaders ModifyClientProperties{..}
          = Core.pure
              ("X-Amz-Target", "WorkspacesService.ModifyClientProperties")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ModifyClientProperties where
        toJSON ModifyClientProperties{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ResourceId" Core..= resourceId),
                  Core.Just ("ClientProperties" Core..= clientProperties)])

instance Core.AWSRequest ModifyClientProperties where
        type Rs ModifyClientProperties = ModifyClientPropertiesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 ModifyClientPropertiesResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkModifyClientPropertiesResponse' smart constructor.
newtype ModifyClientPropertiesResponse = ModifyClientPropertiesResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyClientPropertiesResponse' value with any optional fields omitted.
mkModifyClientPropertiesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ModifyClientPropertiesResponse
mkModifyClientPropertiesResponse responseStatus
  = ModifyClientPropertiesResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcprrsResponseStatus :: Lens.Lens' ModifyClientPropertiesResponse Core.Int
mcprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE mcprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

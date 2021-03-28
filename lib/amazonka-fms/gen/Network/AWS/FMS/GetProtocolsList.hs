{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.GetProtocolsList
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the specified AWS Firewall Manager protocols list.
module Network.AWS.FMS.GetProtocolsList
    (
    -- * Creating a request
      GetProtocolsList (..)
    , mkGetProtocolsList
    -- ** Request lenses
    , gplListId
    , gplDefaultList

    -- * Destructuring the response
    , GetProtocolsListResponse (..)
    , mkGetProtocolsListResponse
    -- ** Response lenses
    , gplrrsProtocolsList
    , gplrrsProtocolsListArn
    , gplrrsResponseStatus
    ) where

import qualified Network.AWS.FMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetProtocolsList' smart constructor.
data GetProtocolsList = GetProtocolsList'
  { listId :: Types.ListId
    -- ^ The ID of the AWS Firewall Manager protocols list that you want the details for.
  , defaultList :: Core.Maybe Core.Bool
    -- ^ Specifies whether the list to retrieve is a default list owned by AWS Firewall Manager.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetProtocolsList' value with any optional fields omitted.
mkGetProtocolsList
    :: Types.ListId -- ^ 'listId'
    -> GetProtocolsList
mkGetProtocolsList listId
  = GetProtocolsList'{listId, defaultList = Core.Nothing}

-- | The ID of the AWS Firewall Manager protocols list that you want the details for.
--
-- /Note:/ Consider using 'listId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gplListId :: Lens.Lens' GetProtocolsList Types.ListId
gplListId = Lens.field @"listId"
{-# INLINEABLE gplListId #-}
{-# DEPRECATED listId "Use generic-lens or generic-optics with 'listId' instead"  #-}

-- | Specifies whether the list to retrieve is a default list owned by AWS Firewall Manager.
--
-- /Note:/ Consider using 'defaultList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gplDefaultList :: Lens.Lens' GetProtocolsList (Core.Maybe Core.Bool)
gplDefaultList = Lens.field @"defaultList"
{-# INLINEABLE gplDefaultList #-}
{-# DEPRECATED defaultList "Use generic-lens or generic-optics with 'defaultList' instead"  #-}

instance Core.ToQuery GetProtocolsList where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetProtocolsList where
        toHeaders GetProtocolsList{..}
          = Core.pure ("X-Amz-Target", "AWSFMS_20180101.GetProtocolsList")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetProtocolsList where
        toJSON GetProtocolsList{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ListId" Core..= listId),
                  ("DefaultList" Core..=) Core.<$> defaultList])

instance Core.AWSRequest GetProtocolsList where
        type Rs GetProtocolsList = GetProtocolsListResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetProtocolsListResponse' Core.<$>
                   (x Core..:? "ProtocolsList") Core.<*> x Core..:? "ProtocolsListArn"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetProtocolsListResponse' smart constructor.
data GetProtocolsListResponse = GetProtocolsListResponse'
  { protocolsList :: Core.Maybe Types.ProtocolsListData
    -- ^ Information about the specified AWS Firewall Manager protocols list.
  , protocolsListArn :: Core.Maybe Types.ResourceArn
    -- ^ The Amazon Resource Name (ARN) of the specified protocols list.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetProtocolsListResponse' value with any optional fields omitted.
mkGetProtocolsListResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetProtocolsListResponse
mkGetProtocolsListResponse responseStatus
  = GetProtocolsListResponse'{protocolsList = Core.Nothing,
                              protocolsListArn = Core.Nothing, responseStatus}

-- | Information about the specified AWS Firewall Manager protocols list.
--
-- /Note:/ Consider using 'protocolsList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gplrrsProtocolsList :: Lens.Lens' GetProtocolsListResponse (Core.Maybe Types.ProtocolsListData)
gplrrsProtocolsList = Lens.field @"protocolsList"
{-# INLINEABLE gplrrsProtocolsList #-}
{-# DEPRECATED protocolsList "Use generic-lens or generic-optics with 'protocolsList' instead"  #-}

-- | The Amazon Resource Name (ARN) of the specified protocols list.
--
-- /Note:/ Consider using 'protocolsListArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gplrrsProtocolsListArn :: Lens.Lens' GetProtocolsListResponse (Core.Maybe Types.ResourceArn)
gplrrsProtocolsListArn = Lens.field @"protocolsListArn"
{-# INLINEABLE gplrrsProtocolsListArn #-}
{-# DEPRECATED protocolsListArn "Use generic-lens or generic-optics with 'protocolsListArn' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gplrrsResponseStatus :: Lens.Lens' GetProtocolsListResponse Core.Int
gplrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gplrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

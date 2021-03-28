{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.PutProtocolsList
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an AWS Firewall Manager protocols list.
module Network.AWS.FMS.PutProtocolsList
    (
    -- * Creating a request
      PutProtocolsList (..)
    , mkPutProtocolsList
    -- ** Request lenses
    , pplProtocolsList
    , pplTagList

    -- * Destructuring the response
    , PutProtocolsListResponse (..)
    , mkPutProtocolsListResponse
    -- ** Response lenses
    , pplrrsProtocolsList
    , pplrrsProtocolsListArn
    , pplrrsResponseStatus
    ) where

import qualified Network.AWS.FMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPutProtocolsList' smart constructor.
data PutProtocolsList = PutProtocolsList'
  { protocolsList :: Types.ProtocolsListData
    -- ^ The details of the AWS Firewall Manager protocols list to be created.
  , tagList :: Core.Maybe [Types.Tag]
    -- ^ The tags associated with the resource.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'PutProtocolsList' value with any optional fields omitted.
mkPutProtocolsList
    :: Types.ProtocolsListData -- ^ 'protocolsList'
    -> PutProtocolsList
mkPutProtocolsList protocolsList
  = PutProtocolsList'{protocolsList, tagList = Core.Nothing}

-- | The details of the AWS Firewall Manager protocols list to be created.
--
-- /Note:/ Consider using 'protocolsList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pplProtocolsList :: Lens.Lens' PutProtocolsList Types.ProtocolsListData
pplProtocolsList = Lens.field @"protocolsList"
{-# INLINEABLE pplProtocolsList #-}
{-# DEPRECATED protocolsList "Use generic-lens or generic-optics with 'protocolsList' instead"  #-}

-- | The tags associated with the resource.
--
-- /Note:/ Consider using 'tagList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pplTagList :: Lens.Lens' PutProtocolsList (Core.Maybe [Types.Tag])
pplTagList = Lens.field @"tagList"
{-# INLINEABLE pplTagList #-}
{-# DEPRECATED tagList "Use generic-lens or generic-optics with 'tagList' instead"  #-}

instance Core.ToQuery PutProtocolsList where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders PutProtocolsList where
        toHeaders PutProtocolsList{..}
          = Core.pure ("X-Amz-Target", "AWSFMS_20180101.PutProtocolsList")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON PutProtocolsList where
        toJSON PutProtocolsList{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ProtocolsList" Core..= protocolsList),
                  ("TagList" Core..=) Core.<$> tagList])

instance Core.AWSRequest PutProtocolsList where
        type Rs PutProtocolsList = PutProtocolsListResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 PutProtocolsListResponse' Core.<$>
                   (x Core..:? "ProtocolsList") Core.<*> x Core..:? "ProtocolsListArn"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkPutProtocolsListResponse' smart constructor.
data PutProtocolsListResponse = PutProtocolsListResponse'
  { protocolsList :: Core.Maybe Types.ProtocolsListData
    -- ^ The details of the AWS Firewall Manager protocols list.
  , protocolsListArn :: Core.Maybe Types.ProtocolsListArn
    -- ^ The Amazon Resource Name (ARN) of the protocols list.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'PutProtocolsListResponse' value with any optional fields omitted.
mkPutProtocolsListResponse
    :: Core.Int -- ^ 'responseStatus'
    -> PutProtocolsListResponse
mkPutProtocolsListResponse responseStatus
  = PutProtocolsListResponse'{protocolsList = Core.Nothing,
                              protocolsListArn = Core.Nothing, responseStatus}

-- | The details of the AWS Firewall Manager protocols list.
--
-- /Note:/ Consider using 'protocolsList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pplrrsProtocolsList :: Lens.Lens' PutProtocolsListResponse (Core.Maybe Types.ProtocolsListData)
pplrrsProtocolsList = Lens.field @"protocolsList"
{-# INLINEABLE pplrrsProtocolsList #-}
{-# DEPRECATED protocolsList "Use generic-lens or generic-optics with 'protocolsList' instead"  #-}

-- | The Amazon Resource Name (ARN) of the protocols list.
--
-- /Note:/ Consider using 'protocolsListArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pplrrsProtocolsListArn :: Lens.Lens' PutProtocolsListResponse (Core.Maybe Types.ProtocolsListArn)
pplrrsProtocolsListArn = Lens.field @"protocolsListArn"
{-# INLINEABLE pplrrsProtocolsListArn #-}
{-# DEPRECATED protocolsListArn "Use generic-lens or generic-optics with 'protocolsListArn' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pplrrsResponseStatus :: Lens.Lens' PutProtocolsListResponse Core.Int
pplrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE pplrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.DescribeContactFlow
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified contact flow.
--
-- You can also create and update contact flows using the <https://docs.aws.amazon.com/connect/latest/adminguide/flow-language.html Amazon Connect Flow language> .
module Network.AWS.Connect.DescribeContactFlow
    (
    -- * Creating a request
      DescribeContactFlow (..)
    , mkDescribeContactFlow
    -- ** Request lenses
    , dcfInstanceId
    , dcfContactFlowId

    -- * Destructuring the response
    , DescribeContactFlowResponse (..)
    , mkDescribeContactFlowResponse
    -- ** Response lenses
    , dcfrrsContactFlow
    , dcfrrsResponseStatus
    ) where

import qualified Network.AWS.Connect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeContactFlow' smart constructor.
data DescribeContactFlow = DescribeContactFlow'
  { instanceId :: Types.InstanceId
    -- ^ The identifier of the Amazon Connect instance.
  , contactFlowId :: Types.ContactFlowId
    -- ^ The identifier of the contact flow.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeContactFlow' value with any optional fields omitted.
mkDescribeContactFlow
    :: Types.InstanceId -- ^ 'instanceId'
    -> Types.ContactFlowId -- ^ 'contactFlowId'
    -> DescribeContactFlow
mkDescribeContactFlow instanceId contactFlowId
  = DescribeContactFlow'{instanceId, contactFlowId}

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcfInstanceId :: Lens.Lens' DescribeContactFlow Types.InstanceId
dcfInstanceId = Lens.field @"instanceId"
{-# INLINEABLE dcfInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

-- | The identifier of the contact flow.
--
-- /Note:/ Consider using 'contactFlowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcfContactFlowId :: Lens.Lens' DescribeContactFlow Types.ContactFlowId
dcfContactFlowId = Lens.field @"contactFlowId"
{-# INLINEABLE dcfContactFlowId #-}
{-# DEPRECATED contactFlowId "Use generic-lens or generic-optics with 'contactFlowId' instead"  #-}

instance Core.ToQuery DescribeContactFlow where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeContactFlow where
        toHeaders DescribeContactFlow{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest DescribeContactFlow where
        type Rs DescribeContactFlow = DescribeContactFlowResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/contact-flows/" Core.<> Core.toText instanceId Core.<> "/"
                             Core.<> Core.toText contactFlowId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeContactFlowResponse' Core.<$>
                   (x Core..:? "ContactFlow") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeContactFlowResponse' smart constructor.
data DescribeContactFlowResponse = DescribeContactFlowResponse'
  { contactFlow :: Core.Maybe Types.ContactFlow
    -- ^ Information about the contact flow.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeContactFlowResponse' value with any optional fields omitted.
mkDescribeContactFlowResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeContactFlowResponse
mkDescribeContactFlowResponse responseStatus
  = DescribeContactFlowResponse'{contactFlow = Core.Nothing,
                                 responseStatus}

-- | Information about the contact flow.
--
-- /Note:/ Consider using 'contactFlow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcfrrsContactFlow :: Lens.Lens' DescribeContactFlowResponse (Core.Maybe Types.ContactFlow)
dcfrrsContactFlow = Lens.field @"contactFlow"
{-# INLINEABLE dcfrrsContactFlow #-}
{-# DEPRECATED contactFlow "Use generic-lens or generic-optics with 'contactFlow' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcfrrsResponseStatus :: Lens.Lens' DescribeContactFlowResponse Core.Int
dcfrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dcfrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

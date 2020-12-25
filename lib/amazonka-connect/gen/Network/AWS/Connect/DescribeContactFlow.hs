{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DescribeContactFlow (..),
    mkDescribeContactFlow,

    -- ** Request lenses
    dcfInstanceId,
    dcfContactFlowId,

    -- * Destructuring the response
    DescribeContactFlowResponse (..),
    mkDescribeContactFlowResponse,

    -- ** Response lenses
    dcfrrsContactFlow,
    dcfrrsResponseStatus,
  )
where

import qualified Network.AWS.Connect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeContactFlow' smart constructor.
data DescribeContactFlow = DescribeContactFlow'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Types.InstanceId,
    -- | The identifier of the contact flow.
    contactFlowId :: Types.ContactFlowId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeContactFlow' value with any optional fields omitted.
mkDescribeContactFlow ::
  -- | 'instanceId'
  Types.InstanceId ->
  -- | 'contactFlowId'
  Types.ContactFlowId ->
  DescribeContactFlow
mkDescribeContactFlow instanceId contactFlowId =
  DescribeContactFlow' {instanceId, contactFlowId}

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcfInstanceId :: Lens.Lens' DescribeContactFlow Types.InstanceId
dcfInstanceId = Lens.field @"instanceId"
{-# DEPRECATED dcfInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The identifier of the contact flow.
--
-- /Note:/ Consider using 'contactFlowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcfContactFlowId :: Lens.Lens' DescribeContactFlow Types.ContactFlowId
dcfContactFlowId = Lens.field @"contactFlowId"
{-# DEPRECATED dcfContactFlowId "Use generic-lens or generic-optics with 'contactFlowId' instead." #-}

instance Core.AWSRequest DescribeContactFlow where
  type Rs DescribeContactFlow = DescribeContactFlowResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/contact-flows/" Core.<> (Core.toText instanceId) Core.<> ("/")
                Core.<> (Core.toText contactFlowId)
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeContactFlowResponse'
            Core.<$> (x Core..:? "ContactFlow") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeContactFlowResponse' smart constructor.
data DescribeContactFlowResponse = DescribeContactFlowResponse'
  { -- | Information about the contact flow.
    contactFlow :: Core.Maybe Types.ContactFlow,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeContactFlowResponse' value with any optional fields omitted.
mkDescribeContactFlowResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeContactFlowResponse
mkDescribeContactFlowResponse responseStatus =
  DescribeContactFlowResponse'
    { contactFlow = Core.Nothing,
      responseStatus
    }

-- | Information about the contact flow.
--
-- /Note:/ Consider using 'contactFlow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcfrrsContactFlow :: Lens.Lens' DescribeContactFlowResponse (Core.Maybe Types.ContactFlow)
dcfrrsContactFlow = Lens.field @"contactFlow"
{-# DEPRECATED dcfrrsContactFlow "Use generic-lens or generic-optics with 'contactFlow' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcfrrsResponseStatus :: Lens.Lens' DescribeContactFlowResponse Core.Int
dcfrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dcfrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

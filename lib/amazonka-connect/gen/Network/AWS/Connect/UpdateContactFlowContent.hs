{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.UpdateContactFlowContent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified contact flow.
--
-- You can also create and update contact flows using the <https://docs.aws.amazon.com/connect/latest/adminguide/flow-language.html Amazon Connect Flow language> .
module Network.AWS.Connect.UpdateContactFlowContent
  ( -- * Creating a request
    UpdateContactFlowContent (..),
    mkUpdateContactFlowContent,

    -- ** Request lenses
    ucfcInstanceId,
    ucfcContactFlowId,
    ucfcContent,

    -- * Destructuring the response
    UpdateContactFlowContentResponse (..),
    mkUpdateContactFlowContentResponse,
  )
where

import qualified Network.AWS.Connect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateContactFlowContent' smart constructor.
data UpdateContactFlowContent = UpdateContactFlowContent'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Types.InstanceId,
    -- | The identifier of the contact flow.
    contactFlowId :: Types.ContactFlowId,
    -- | The JSON string that represents contact flow’s content. For an example, see <https://docs.aws.amazon.com/connect/latest/adminguide/flow-language-example.html Example contact flow in Amazon Connect Flow language> in the /Amazon Connect Administrator Guide/ .
    content :: Types.ContactFlowContent
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateContactFlowContent' value with any optional fields omitted.
mkUpdateContactFlowContent ::
  -- | 'instanceId'
  Types.InstanceId ->
  -- | 'contactFlowId'
  Types.ContactFlowId ->
  -- | 'content'
  Types.ContactFlowContent ->
  UpdateContactFlowContent
mkUpdateContactFlowContent instanceId contactFlowId content =
  UpdateContactFlowContent' {instanceId, contactFlowId, content}

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucfcInstanceId :: Lens.Lens' UpdateContactFlowContent Types.InstanceId
ucfcInstanceId = Lens.field @"instanceId"
{-# DEPRECATED ucfcInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The identifier of the contact flow.
--
-- /Note:/ Consider using 'contactFlowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucfcContactFlowId :: Lens.Lens' UpdateContactFlowContent Types.ContactFlowId
ucfcContactFlowId = Lens.field @"contactFlowId"
{-# DEPRECATED ucfcContactFlowId "Use generic-lens or generic-optics with 'contactFlowId' instead." #-}

-- | The JSON string that represents contact flow’s content. For an example, see <https://docs.aws.amazon.com/connect/latest/adminguide/flow-language-example.html Example contact flow in Amazon Connect Flow language> in the /Amazon Connect Administrator Guide/ .
--
-- /Note:/ Consider using 'content' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucfcContent :: Lens.Lens' UpdateContactFlowContent Types.ContactFlowContent
ucfcContent = Lens.field @"content"
{-# DEPRECATED ucfcContent "Use generic-lens or generic-optics with 'content' instead." #-}

instance Core.FromJSON UpdateContactFlowContent where
  toJSON UpdateContactFlowContent {..} =
    Core.object
      (Core.catMaybes [Core.Just ("Content" Core..= content)])

instance Core.AWSRequest UpdateContactFlowContent where
  type Rs UpdateContactFlowContent = UpdateContactFlowContentResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ( "/contact-flows/" Core.<> (Core.toText instanceId) Core.<> ("/")
                Core.<> (Core.toText contactFlowId)
                Core.<> ("/content")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull UpdateContactFlowContentResponse'

-- | /See:/ 'mkUpdateContactFlowContentResponse' smart constructor.
data UpdateContactFlowContentResponse = UpdateContactFlowContentResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateContactFlowContentResponse' value with any optional fields omitted.
mkUpdateContactFlowContentResponse ::
  UpdateContactFlowContentResponse
mkUpdateContactFlowContentResponse =
  UpdateContactFlowContentResponse'

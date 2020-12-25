{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.DeleteWebhook
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a previously created webhook by name. Deleting the webhook stops AWS CodePipeline from starting a pipeline every time an external event occurs. The API returns successfully when trying to delete a webhook that is already deleted. If a deleted webhook is re-created by calling PutWebhook with the same name, it will have a different URL.
module Network.AWS.CodePipeline.DeleteWebhook
  ( -- * Creating a request
    DeleteWebhook (..),
    mkDeleteWebhook,

    -- ** Request lenses
    dwName,

    -- * Destructuring the response
    DeleteWebhookResponse (..),
    mkDeleteWebhookResponse,

    -- ** Response lenses
    dwrrsResponseStatus,
  )
where

import qualified Network.AWS.CodePipeline.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteWebhook' smart constructor.
newtype DeleteWebhook = DeleteWebhook'
  { -- | The name of the webhook you want to delete.
    name :: Types.WebhookName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteWebhook' value with any optional fields omitted.
mkDeleteWebhook ::
  -- | 'name'
  Types.WebhookName ->
  DeleteWebhook
mkDeleteWebhook name = DeleteWebhook' {name}

-- | The name of the webhook you want to delete.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwName :: Lens.Lens' DeleteWebhook Types.WebhookName
dwName = Lens.field @"name"
{-# DEPRECATED dwName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON DeleteWebhook where
  toJSON DeleteWebhook {..} =
    Core.object (Core.catMaybes [Core.Just ("name" Core..= name)])

instance Core.AWSRequest DeleteWebhook where
  type Rs DeleteWebhook = DeleteWebhookResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "CodePipeline_20150709.DeleteWebhook")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteWebhookResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteWebhookResponse' smart constructor.
newtype DeleteWebhookResponse = DeleteWebhookResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteWebhookResponse' value with any optional fields omitted.
mkDeleteWebhookResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteWebhookResponse
mkDeleteWebhookResponse responseStatus =
  DeleteWebhookResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwrrsResponseStatus :: Lens.Lens' DeleteWebhookResponse Core.Int
dwrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dwrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

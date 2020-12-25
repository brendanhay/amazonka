{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.CreatePushTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a message template for messages that are sent through a push notification channel.
module Network.AWS.Pinpoint.CreatePushTemplate
  ( -- * Creating a request
    CreatePushTemplate (..),
    mkCreatePushTemplate,

    -- ** Request lenses
    cptTemplateName,
    cptPushNotificationTemplateRequest,

    -- * Destructuring the response
    CreatePushTemplateResponse (..),
    mkCreatePushTemplateResponse,

    -- ** Response lenses
    cptrrsCreateTemplateMessageBody,
    cptrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreatePushTemplate' smart constructor.
data CreatePushTemplate = CreatePushTemplate'
  { -- | The name of the message template. A template name must start with an alphanumeric character and can contain a maximum of 128 characters. The characters can be alphanumeric characters, underscores (_), or hyphens (-). Template names are case sensitive.
    templateName :: Core.Text,
    pushNotificationTemplateRequest :: Types.PushNotificationTemplateRequest
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreatePushTemplate' value with any optional fields omitted.
mkCreatePushTemplate ::
  -- | 'templateName'
  Core.Text ->
  -- | 'pushNotificationTemplateRequest'
  Types.PushNotificationTemplateRequest ->
  CreatePushTemplate
mkCreatePushTemplate templateName pushNotificationTemplateRequest =
  CreatePushTemplate'
    { templateName,
      pushNotificationTemplateRequest
    }

-- | The name of the message template. A template name must start with an alphanumeric character and can contain a maximum of 128 characters. The characters can be alphanumeric characters, underscores (_), or hyphens (-). Template names are case sensitive.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cptTemplateName :: Lens.Lens' CreatePushTemplate Core.Text
cptTemplateName = Lens.field @"templateName"
{-# DEPRECATED cptTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'pushNotificationTemplateRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cptPushNotificationTemplateRequest :: Lens.Lens' CreatePushTemplate Types.PushNotificationTemplateRequest
cptPushNotificationTemplateRequest = Lens.field @"pushNotificationTemplateRequest"
{-# DEPRECATED cptPushNotificationTemplateRequest "Use generic-lens or generic-optics with 'pushNotificationTemplateRequest' instead." #-}

instance Core.FromJSON CreatePushTemplate where
  toJSON CreatePushTemplate {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "PushNotificationTemplateRequest"
                  Core..= pushNotificationTemplateRequest
              )
          ]
      )

instance Core.AWSRequest CreatePushTemplate where
  type Rs CreatePushTemplate = CreatePushTemplateResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ( "/v1/templates/" Core.<> (Core.toText templateName)
                Core.<> ("/push")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreatePushTemplateResponse'
            Core.<$> (Core.eitherParseJSON x) Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreatePushTemplateResponse' smart constructor.
data CreatePushTemplateResponse = CreatePushTemplateResponse'
  { createTemplateMessageBody :: Types.CreateTemplateMessageBody,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreatePushTemplateResponse' value with any optional fields omitted.
mkCreatePushTemplateResponse ::
  -- | 'createTemplateMessageBody'
  Types.CreateTemplateMessageBody ->
  -- | 'responseStatus'
  Core.Int ->
  CreatePushTemplateResponse
mkCreatePushTemplateResponse
  createTemplateMessageBody
  responseStatus =
    CreatePushTemplateResponse'
      { createTemplateMessageBody,
        responseStatus
      }

-- | Undocumented field.
--
-- /Note:/ Consider using 'createTemplateMessageBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cptrrsCreateTemplateMessageBody :: Lens.Lens' CreatePushTemplateResponse Types.CreateTemplateMessageBody
cptrrsCreateTemplateMessageBody = Lens.field @"createTemplateMessageBody"
{-# DEPRECATED cptrrsCreateTemplateMessageBody "Use generic-lens or generic-optics with 'createTemplateMessageBody' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cptrrsResponseStatus :: Lens.Lens' CreatePushTemplateResponse Core.Int
cptrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cptrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

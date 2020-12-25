{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.CreateSmsTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a message template for messages that are sent through the SMS channel.
module Network.AWS.Pinpoint.CreateSmsTemplate
  ( -- * Creating a request
    CreateSmsTemplate (..),
    mkCreateSmsTemplate,

    -- ** Request lenses
    cstTemplateName,
    cstSMSTemplateRequest,

    -- * Destructuring the response
    CreateSmsTemplateResponse (..),
    mkCreateSmsTemplateResponse,

    -- ** Response lenses
    cstrrsCreateTemplateMessageBody,
    cstrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateSmsTemplate' smart constructor.
data CreateSmsTemplate = CreateSmsTemplate'
  { -- | The name of the message template. A template name must start with an alphanumeric character and can contain a maximum of 128 characters. The characters can be alphanumeric characters, underscores (_), or hyphens (-). Template names are case sensitive.
    templateName :: Core.Text,
    sMSTemplateRequest :: Types.SMSTemplateRequest
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateSmsTemplate' value with any optional fields omitted.
mkCreateSmsTemplate ::
  -- | 'templateName'
  Core.Text ->
  -- | 'sMSTemplateRequest'
  Types.SMSTemplateRequest ->
  CreateSmsTemplate
mkCreateSmsTemplate templateName sMSTemplateRequest =
  CreateSmsTemplate' {templateName, sMSTemplateRequest}

-- | The name of the message template. A template name must start with an alphanumeric character and can contain a maximum of 128 characters. The characters can be alphanumeric characters, underscores (_), or hyphens (-). Template names are case sensitive.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cstTemplateName :: Lens.Lens' CreateSmsTemplate Core.Text
cstTemplateName = Lens.field @"templateName"
{-# DEPRECATED cstTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'sMSTemplateRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cstSMSTemplateRequest :: Lens.Lens' CreateSmsTemplate Types.SMSTemplateRequest
cstSMSTemplateRequest = Lens.field @"sMSTemplateRequest"
{-# DEPRECATED cstSMSTemplateRequest "Use generic-lens or generic-optics with 'sMSTemplateRequest' instead." #-}

instance Core.FromJSON CreateSmsTemplate where
  toJSON CreateSmsTemplate {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("SMSTemplateRequest" Core..= sMSTemplateRequest)]
      )

instance Core.AWSRequest CreateSmsTemplate where
  type Rs CreateSmsTemplate = CreateSmsTemplateResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ( "/v1/templates/" Core.<> (Core.toText templateName)
                Core.<> ("/sms")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateSmsTemplateResponse'
            Core.<$> (Core.eitherParseJSON x) Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateSmsTemplateResponse' smart constructor.
data CreateSmsTemplateResponse = CreateSmsTemplateResponse'
  { createTemplateMessageBody :: Types.CreateTemplateMessageBody,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateSmsTemplateResponse' value with any optional fields omitted.
mkCreateSmsTemplateResponse ::
  -- | 'createTemplateMessageBody'
  Types.CreateTemplateMessageBody ->
  -- | 'responseStatus'
  Core.Int ->
  CreateSmsTemplateResponse
mkCreateSmsTemplateResponse
  createTemplateMessageBody
  responseStatus =
    CreateSmsTemplateResponse'
      { createTemplateMessageBody,
        responseStatus
      }

-- | Undocumented field.
--
-- /Note:/ Consider using 'createTemplateMessageBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cstrrsCreateTemplateMessageBody :: Lens.Lens' CreateSmsTemplateResponse Types.CreateTemplateMessageBody
cstrrsCreateTemplateMessageBody = Lens.field @"createTemplateMessageBody"
{-# DEPRECATED cstrrsCreateTemplateMessageBody "Use generic-lens or generic-optics with 'createTemplateMessageBody' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cstrrsResponseStatus :: Lens.Lens' CreateSmsTemplateResponse Core.Int
cstrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cstrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.CreateEmailTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a message template for messages that are sent through the email channel.
module Network.AWS.Pinpoint.CreateEmailTemplate
  ( -- * Creating a request
    CreateEmailTemplate (..),
    mkCreateEmailTemplate,

    -- ** Request lenses
    cetTemplateName,
    cetEmailTemplateRequest,

    -- * Destructuring the response
    CreateEmailTemplateResponse (..),
    mkCreateEmailTemplateResponse,

    -- ** Response lenses
    cetrrsCreateTemplateMessageBody,
    cetrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateEmailTemplate' smart constructor.
data CreateEmailTemplate = CreateEmailTemplate'
  { -- | The name of the message template. A template name must start with an alphanumeric character and can contain a maximum of 128 characters. The characters can be alphanumeric characters, underscores (_), or hyphens (-). Template names are case sensitive.
    templateName :: Core.Text,
    emailTemplateRequest :: Types.EmailTemplateRequest
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateEmailTemplate' value with any optional fields omitted.
mkCreateEmailTemplate ::
  -- | 'templateName'
  Core.Text ->
  -- | 'emailTemplateRequest'
  Types.EmailTemplateRequest ->
  CreateEmailTemplate
mkCreateEmailTemplate templateName emailTemplateRequest =
  CreateEmailTemplate' {templateName, emailTemplateRequest}

-- | The name of the message template. A template name must start with an alphanumeric character and can contain a maximum of 128 characters. The characters can be alphanumeric characters, underscores (_), or hyphens (-). Template names are case sensitive.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cetTemplateName :: Lens.Lens' CreateEmailTemplate Core.Text
cetTemplateName = Lens.field @"templateName"
{-# DEPRECATED cetTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'emailTemplateRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cetEmailTemplateRequest :: Lens.Lens' CreateEmailTemplate Types.EmailTemplateRequest
cetEmailTemplateRequest = Lens.field @"emailTemplateRequest"
{-# DEPRECATED cetEmailTemplateRequest "Use generic-lens or generic-optics with 'emailTemplateRequest' instead." #-}

instance Core.FromJSON CreateEmailTemplate where
  toJSON CreateEmailTemplate {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("EmailTemplateRequest" Core..= emailTemplateRequest)]
      )

instance Core.AWSRequest CreateEmailTemplate where
  type Rs CreateEmailTemplate = CreateEmailTemplateResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ( "/v1/templates/" Core.<> (Core.toText templateName)
                Core.<> ("/email")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateEmailTemplateResponse'
            Core.<$> (Core.eitherParseJSON x) Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateEmailTemplateResponse' smart constructor.
data CreateEmailTemplateResponse = CreateEmailTemplateResponse'
  { createTemplateMessageBody :: Types.CreateTemplateMessageBody,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateEmailTemplateResponse' value with any optional fields omitted.
mkCreateEmailTemplateResponse ::
  -- | 'createTemplateMessageBody'
  Types.CreateTemplateMessageBody ->
  -- | 'responseStatus'
  Core.Int ->
  CreateEmailTemplateResponse
mkCreateEmailTemplateResponse
  createTemplateMessageBody
  responseStatus =
    CreateEmailTemplateResponse'
      { createTemplateMessageBody,
        responseStatus
      }

-- | Undocumented field.
--
-- /Note:/ Consider using 'createTemplateMessageBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cetrrsCreateTemplateMessageBody :: Lens.Lens' CreateEmailTemplateResponse Types.CreateTemplateMessageBody
cetrrsCreateTemplateMessageBody = Lens.field @"createTemplateMessageBody"
{-# DEPRECATED cetrrsCreateTemplateMessageBody "Use generic-lens or generic-optics with 'createTemplateMessageBody' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cetrrsResponseStatus :: Lens.Lens' CreateEmailTemplateResponse Core.Int
cetrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cetrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

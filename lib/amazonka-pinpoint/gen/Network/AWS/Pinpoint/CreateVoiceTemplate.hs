{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.CreateVoiceTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a message template for messages that are sent through the voice channel.
module Network.AWS.Pinpoint.CreateVoiceTemplate
  ( -- * Creating a request
    CreateVoiceTemplate (..),
    mkCreateVoiceTemplate,

    -- ** Request lenses
    cvtTemplateName,
    cvtVoiceTemplateRequest,

    -- * Destructuring the response
    CreateVoiceTemplateResponse (..),
    mkCreateVoiceTemplateResponse,

    -- ** Response lenses
    cvtrrsCreateTemplateMessageBody,
    cvtrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateVoiceTemplate' smart constructor.
data CreateVoiceTemplate = CreateVoiceTemplate'
  { -- | The name of the message template. A template name must start with an alphanumeric character and can contain a maximum of 128 characters. The characters can be alphanumeric characters, underscores (_), or hyphens (-). Template names are case sensitive.
    templateName :: Core.Text,
    voiceTemplateRequest :: Types.VoiceTemplateRequest
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateVoiceTemplate' value with any optional fields omitted.
mkCreateVoiceTemplate ::
  -- | 'templateName'
  Core.Text ->
  -- | 'voiceTemplateRequest'
  Types.VoiceTemplateRequest ->
  CreateVoiceTemplate
mkCreateVoiceTemplate templateName voiceTemplateRequest =
  CreateVoiceTemplate' {templateName, voiceTemplateRequest}

-- | The name of the message template. A template name must start with an alphanumeric character and can contain a maximum of 128 characters. The characters can be alphanumeric characters, underscores (_), or hyphens (-). Template names are case sensitive.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvtTemplateName :: Lens.Lens' CreateVoiceTemplate Core.Text
cvtTemplateName = Lens.field @"templateName"
{-# DEPRECATED cvtTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'voiceTemplateRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvtVoiceTemplateRequest :: Lens.Lens' CreateVoiceTemplate Types.VoiceTemplateRequest
cvtVoiceTemplateRequest = Lens.field @"voiceTemplateRequest"
{-# DEPRECATED cvtVoiceTemplateRequest "Use generic-lens or generic-optics with 'voiceTemplateRequest' instead." #-}

instance Core.FromJSON CreateVoiceTemplate where
  toJSON CreateVoiceTemplate {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("VoiceTemplateRequest" Core..= voiceTemplateRequest)]
      )

instance Core.AWSRequest CreateVoiceTemplate where
  type Rs CreateVoiceTemplate = CreateVoiceTemplateResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ( "/v1/templates/" Core.<> (Core.toText templateName)
                Core.<> ("/voice")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateVoiceTemplateResponse'
            Core.<$> (Core.eitherParseJSON x) Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateVoiceTemplateResponse' smart constructor.
data CreateVoiceTemplateResponse = CreateVoiceTemplateResponse'
  { createTemplateMessageBody :: Types.CreateTemplateMessageBody,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateVoiceTemplateResponse' value with any optional fields omitted.
mkCreateVoiceTemplateResponse ::
  -- | 'createTemplateMessageBody'
  Types.CreateTemplateMessageBody ->
  -- | 'responseStatus'
  Core.Int ->
  CreateVoiceTemplateResponse
mkCreateVoiceTemplateResponse
  createTemplateMessageBody
  responseStatus =
    CreateVoiceTemplateResponse'
      { createTemplateMessageBody,
        responseStatus
      }

-- | Undocumented field.
--
-- /Note:/ Consider using 'createTemplateMessageBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvtrrsCreateTemplateMessageBody :: Lens.Lens' CreateVoiceTemplateResponse Types.CreateTemplateMessageBody
cvtrrsCreateTemplateMessageBody = Lens.field @"createTemplateMessageBody"
{-# DEPRECATED cvtrrsCreateTemplateMessageBody "Use generic-lens or generic-optics with 'createTemplateMessageBody' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvtrrsResponseStatus :: Lens.Lens' CreateVoiceTemplateResponse Core.Int
cvtrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cvtrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

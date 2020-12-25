{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.UpdateVoiceTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing message template for messages that are sent through the voice channel.
module Network.AWS.Pinpoint.UpdateVoiceTemplate
  ( -- * Creating a request
    UpdateVoiceTemplate (..),
    mkUpdateVoiceTemplate,

    -- ** Request lenses
    uvtTemplateName,
    uvtVoiceTemplateRequest,
    uvtCreateNewVersion,
    uvtVersion,

    -- * Destructuring the response
    UpdateVoiceTemplateResponse (..),
    mkUpdateVoiceTemplateResponse,

    -- ** Response lenses
    uvtrrsMessageBody,
    uvtrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateVoiceTemplate' smart constructor.
data UpdateVoiceTemplate = UpdateVoiceTemplate'
  { -- | The name of the message template. A template name must start with an alphanumeric character and can contain a maximum of 128 characters. The characters can be alphanumeric characters, underscores (_), or hyphens (-). Template names are case sensitive.
    templateName :: Core.Text,
    voiceTemplateRequest :: Types.VoiceTemplateRequest,
    -- | Specifies whether to save the updates as a new version of the message template. Valid values are: true, save the updates as a new version; and, false, save the updates to (overwrite) the latest existing version of the template.
    --
    -- If you don't specify a value for this parameter, Amazon Pinpoint saves the updates to (overwrites) the latest existing version of the template. If you specify a value of true for this parameter, don't specify a value for the version parameter. Otherwise, an error will occur.
    createNewVersion :: Core.Maybe Core.Bool,
    -- | The unique identifier for the version of the message template to update, retrieve information about, or delete. To retrieve identifiers and other information for all the versions of a template, use the <link>Template Versions resource.
    --
    -- If specified, this value must match the identifier for an existing template version. If specified for an update operation, this value must match the identifier for the latest existing version of the template. This restriction helps ensure that race conditions don't occur.
    -- If you don't specify a value for this parameter, Amazon Pinpoint does the following:
    --
    --     * For a get operation, retrieves information about the active version of the template.
    --
    --
    --     * For an update operation, saves the updates to (overwrites) the latest existing version of the template, if the create-new-version parameter isn't used or is set to false.
    --
    --
    --     * For a delete operation, deletes the template, including all versions of the template.
    version :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateVoiceTemplate' value with any optional fields omitted.
mkUpdateVoiceTemplate ::
  -- | 'templateName'
  Core.Text ->
  -- | 'voiceTemplateRequest'
  Types.VoiceTemplateRequest ->
  UpdateVoiceTemplate
mkUpdateVoiceTemplate templateName voiceTemplateRequest =
  UpdateVoiceTemplate'
    { templateName,
      voiceTemplateRequest,
      createNewVersion = Core.Nothing,
      version = Core.Nothing
    }

-- | The name of the message template. A template name must start with an alphanumeric character and can contain a maximum of 128 characters. The characters can be alphanumeric characters, underscores (_), or hyphens (-). Template names are case sensitive.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uvtTemplateName :: Lens.Lens' UpdateVoiceTemplate Core.Text
uvtTemplateName = Lens.field @"templateName"
{-# DEPRECATED uvtTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'voiceTemplateRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uvtVoiceTemplateRequest :: Lens.Lens' UpdateVoiceTemplate Types.VoiceTemplateRequest
uvtVoiceTemplateRequest = Lens.field @"voiceTemplateRequest"
{-# DEPRECATED uvtVoiceTemplateRequest "Use generic-lens or generic-optics with 'voiceTemplateRequest' instead." #-}

-- | Specifies whether to save the updates as a new version of the message template. Valid values are: true, save the updates as a new version; and, false, save the updates to (overwrite) the latest existing version of the template.
--
-- If you don't specify a value for this parameter, Amazon Pinpoint saves the updates to (overwrites) the latest existing version of the template. If you specify a value of true for this parameter, don't specify a value for the version parameter. Otherwise, an error will occur.
--
-- /Note:/ Consider using 'createNewVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uvtCreateNewVersion :: Lens.Lens' UpdateVoiceTemplate (Core.Maybe Core.Bool)
uvtCreateNewVersion = Lens.field @"createNewVersion"
{-# DEPRECATED uvtCreateNewVersion "Use generic-lens or generic-optics with 'createNewVersion' instead." #-}

-- | The unique identifier for the version of the message template to update, retrieve information about, or delete. To retrieve identifiers and other information for all the versions of a template, use the <link>Template Versions resource.
--
-- If specified, this value must match the identifier for an existing template version. If specified for an update operation, this value must match the identifier for the latest existing version of the template. This restriction helps ensure that race conditions don't occur.
-- If you don't specify a value for this parameter, Amazon Pinpoint does the following:
--
--     * For a get operation, retrieves information about the active version of the template.
--
--
--     * For an update operation, saves the updates to (overwrites) the latest existing version of the template, if the create-new-version parameter isn't used or is set to false.
--
--
--     * For a delete operation, deletes the template, including all versions of the template.
--
--
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uvtVersion :: Lens.Lens' UpdateVoiceTemplate (Core.Maybe Core.Text)
uvtVersion = Lens.field @"version"
{-# DEPRECATED uvtVersion "Use generic-lens or generic-optics with 'version' instead." #-}

instance Core.FromJSON UpdateVoiceTemplate where
  toJSON UpdateVoiceTemplate {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("VoiceTemplateRequest" Core..= voiceTemplateRequest)]
      )

instance Core.AWSRequest UpdateVoiceTemplate where
  type Rs UpdateVoiceTemplate = UpdateVoiceTemplateResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath =
          Core.rawPath
            ( "/v1/templates/" Core.<> (Core.toText templateName)
                Core.<> ("/voice")
            ),
        Core._rqQuery =
          Core.toQueryValue "create-new-version" Core.<$> createNewVersion
            Core.<> (Core.toQueryValue "version" Core.<$> version),
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateVoiceTemplateResponse'
            Core.<$> (Core.eitherParseJSON x) Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateVoiceTemplateResponse' smart constructor.
data UpdateVoiceTemplateResponse = UpdateVoiceTemplateResponse'
  { messageBody :: Types.MessageBody,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateVoiceTemplateResponse' value with any optional fields omitted.
mkUpdateVoiceTemplateResponse ::
  -- | 'messageBody'
  Types.MessageBody ->
  -- | 'responseStatus'
  Core.Int ->
  UpdateVoiceTemplateResponse
mkUpdateVoiceTemplateResponse messageBody responseStatus =
  UpdateVoiceTemplateResponse' {messageBody, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'messageBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uvtrrsMessageBody :: Lens.Lens' UpdateVoiceTemplateResponse Types.MessageBody
uvtrrsMessageBody = Lens.field @"messageBody"
{-# DEPRECATED uvtrrsMessageBody "Use generic-lens or generic-optics with 'messageBody' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uvtrrsResponseStatus :: Lens.Lens' UpdateVoiceTemplateResponse Core.Int
uvtrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED uvtrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

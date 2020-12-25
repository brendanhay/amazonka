{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.UpdateSmsTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing message template for messages that are sent through the SMS channel.
module Network.AWS.Pinpoint.UpdateSmsTemplate
  ( -- * Creating a request
    UpdateSmsTemplate (..),
    mkUpdateSmsTemplate,

    -- ** Request lenses
    ustTemplateName,
    ustSMSTemplateRequest,
    ustCreateNewVersion,
    ustVersion,

    -- * Destructuring the response
    UpdateSmsTemplateResponse (..),
    mkUpdateSmsTemplateResponse,

    -- ** Response lenses
    ustrrsMessageBody,
    ustrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateSmsTemplate' smart constructor.
data UpdateSmsTemplate = UpdateSmsTemplate'
  { -- | The name of the message template. A template name must start with an alphanumeric character and can contain a maximum of 128 characters. The characters can be alphanumeric characters, underscores (_), or hyphens (-). Template names are case sensitive.
    templateName :: Core.Text,
    sMSTemplateRequest :: Types.SMSTemplateRequest,
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

-- | Creates a 'UpdateSmsTemplate' value with any optional fields omitted.
mkUpdateSmsTemplate ::
  -- | 'templateName'
  Core.Text ->
  -- | 'sMSTemplateRequest'
  Types.SMSTemplateRequest ->
  UpdateSmsTemplate
mkUpdateSmsTemplate templateName sMSTemplateRequest =
  UpdateSmsTemplate'
    { templateName,
      sMSTemplateRequest,
      createNewVersion = Core.Nothing,
      version = Core.Nothing
    }

-- | The name of the message template. A template name must start with an alphanumeric character and can contain a maximum of 128 characters. The characters can be alphanumeric characters, underscores (_), or hyphens (-). Template names are case sensitive.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ustTemplateName :: Lens.Lens' UpdateSmsTemplate Core.Text
ustTemplateName = Lens.field @"templateName"
{-# DEPRECATED ustTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'sMSTemplateRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ustSMSTemplateRequest :: Lens.Lens' UpdateSmsTemplate Types.SMSTemplateRequest
ustSMSTemplateRequest = Lens.field @"sMSTemplateRequest"
{-# DEPRECATED ustSMSTemplateRequest "Use generic-lens or generic-optics with 'sMSTemplateRequest' instead." #-}

-- | Specifies whether to save the updates as a new version of the message template. Valid values are: true, save the updates as a new version; and, false, save the updates to (overwrite) the latest existing version of the template.
--
-- If you don't specify a value for this parameter, Amazon Pinpoint saves the updates to (overwrites) the latest existing version of the template. If you specify a value of true for this parameter, don't specify a value for the version parameter. Otherwise, an error will occur.
--
-- /Note:/ Consider using 'createNewVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ustCreateNewVersion :: Lens.Lens' UpdateSmsTemplate (Core.Maybe Core.Bool)
ustCreateNewVersion = Lens.field @"createNewVersion"
{-# DEPRECATED ustCreateNewVersion "Use generic-lens or generic-optics with 'createNewVersion' instead." #-}

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
ustVersion :: Lens.Lens' UpdateSmsTemplate (Core.Maybe Core.Text)
ustVersion = Lens.field @"version"
{-# DEPRECATED ustVersion "Use generic-lens or generic-optics with 'version' instead." #-}

instance Core.FromJSON UpdateSmsTemplate where
  toJSON UpdateSmsTemplate {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("SMSTemplateRequest" Core..= sMSTemplateRequest)]
      )

instance Core.AWSRequest UpdateSmsTemplate where
  type Rs UpdateSmsTemplate = UpdateSmsTemplateResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath =
          Core.rawPath
            ( "/v1/templates/" Core.<> (Core.toText templateName)
                Core.<> ("/sms")
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
          UpdateSmsTemplateResponse'
            Core.<$> (Core.eitherParseJSON x) Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateSmsTemplateResponse' smart constructor.
data UpdateSmsTemplateResponse = UpdateSmsTemplateResponse'
  { messageBody :: Types.MessageBody,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateSmsTemplateResponse' value with any optional fields omitted.
mkUpdateSmsTemplateResponse ::
  -- | 'messageBody'
  Types.MessageBody ->
  -- | 'responseStatus'
  Core.Int ->
  UpdateSmsTemplateResponse
mkUpdateSmsTemplateResponse messageBody responseStatus =
  UpdateSmsTemplateResponse' {messageBody, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'messageBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ustrrsMessageBody :: Lens.Lens' UpdateSmsTemplateResponse Types.MessageBody
ustrrsMessageBody = Lens.field @"messageBody"
{-# DEPRECATED ustrrsMessageBody "Use generic-lens or generic-optics with 'messageBody' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ustrrsResponseStatus :: Lens.Lens' UpdateSmsTemplateResponse Core.Int
ustrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ustrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

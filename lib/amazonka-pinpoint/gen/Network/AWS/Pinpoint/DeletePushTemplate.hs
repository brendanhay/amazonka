{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.DeletePushTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a message template for messages that were sent through a push notification channel.
module Network.AWS.Pinpoint.DeletePushTemplate
  ( -- * Creating a request
    DeletePushTemplate (..),
    mkDeletePushTemplate,

    -- ** Request lenses
    dptTemplateName,
    dptVersion,

    -- * Destructuring the response
    DeletePushTemplateResponse (..),
    mkDeletePushTemplateResponse,

    -- ** Response lenses
    dptrrsMessageBody,
    dptrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeletePushTemplate' smart constructor.
data DeletePushTemplate = DeletePushTemplate'
  { -- | The name of the message template. A template name must start with an alphanumeric character and can contain a maximum of 128 characters. The characters can be alphanumeric characters, underscores (_), or hyphens (-). Template names are case sensitive.
    templateName :: Core.Text,
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

-- | Creates a 'DeletePushTemplate' value with any optional fields omitted.
mkDeletePushTemplate ::
  -- | 'templateName'
  Core.Text ->
  DeletePushTemplate
mkDeletePushTemplate templateName =
  DeletePushTemplate' {templateName, version = Core.Nothing}

-- | The name of the message template. A template name must start with an alphanumeric character and can contain a maximum of 128 characters. The characters can be alphanumeric characters, underscores (_), or hyphens (-). Template names are case sensitive.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dptTemplateName :: Lens.Lens' DeletePushTemplate Core.Text
dptTemplateName = Lens.field @"templateName"
{-# DEPRECATED dptTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

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
dptVersion :: Lens.Lens' DeletePushTemplate (Core.Maybe Core.Text)
dptVersion = Lens.field @"version"
{-# DEPRECATED dptVersion "Use generic-lens or generic-optics with 'version' instead." #-}

instance Core.AWSRequest DeletePushTemplate where
  type Rs DeletePushTemplate = DeletePushTemplateResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.DELETE,
        Core._rqPath =
          Core.rawPath
            ( "/v1/templates/" Core.<> (Core.toText templateName)
                Core.<> ("/push")
            ),
        Core._rqQuery = Core.toQueryValue "version" Core.<$> version,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DeletePushTemplateResponse'
            Core.<$> (Core.eitherParseJSON x) Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeletePushTemplateResponse' smart constructor.
data DeletePushTemplateResponse = DeletePushTemplateResponse'
  { messageBody :: Types.MessageBody,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeletePushTemplateResponse' value with any optional fields omitted.
mkDeletePushTemplateResponse ::
  -- | 'messageBody'
  Types.MessageBody ->
  -- | 'responseStatus'
  Core.Int ->
  DeletePushTemplateResponse
mkDeletePushTemplateResponse messageBody responseStatus =
  DeletePushTemplateResponse' {messageBody, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'messageBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dptrrsMessageBody :: Lens.Lens' DeletePushTemplateResponse Types.MessageBody
dptrrsMessageBody = Lens.field @"messageBody"
{-# DEPRECATED dptrrsMessageBody "Use generic-lens or generic-optics with 'messageBody' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dptrrsResponseStatus :: Lens.Lens' DeletePushTemplateResponse Core.Int
dptrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dptrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

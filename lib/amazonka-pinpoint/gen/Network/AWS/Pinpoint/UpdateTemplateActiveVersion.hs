{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.UpdateTemplateActiveVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the status of a specific version of a message template to /active/ .
module Network.AWS.Pinpoint.UpdateTemplateActiveVersion
  ( -- * Creating a request
    UpdateTemplateActiveVersion (..),
    mkUpdateTemplateActiveVersion,

    -- ** Request lenses
    utavTemplateName,
    utavTemplateType,
    utavTemplateActiveVersionRequest,

    -- * Destructuring the response
    UpdateTemplateActiveVersionResponse (..),
    mkUpdateTemplateActiveVersionResponse,

    -- ** Response lenses
    utavrrsMessageBody,
    utavrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateTemplateActiveVersion' smart constructor.
data UpdateTemplateActiveVersion = UpdateTemplateActiveVersion'
  { -- | The name of the message template. A template name must start with an alphanumeric character and can contain a maximum of 128 characters. The characters can be alphanumeric characters, underscores (_), or hyphens (-). Template names are case sensitive.
    templateName :: Core.Text,
    -- | The type of channel that the message template is designed for. Valid values are: EMAIL, PUSH, SMS, and VOICE.
    templateType :: Core.Text,
    templateActiveVersionRequest :: Types.TemplateActiveVersionRequest
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateTemplateActiveVersion' value with any optional fields omitted.
mkUpdateTemplateActiveVersion ::
  -- | 'templateName'
  Core.Text ->
  -- | 'templateType'
  Core.Text ->
  -- | 'templateActiveVersionRequest'
  Types.TemplateActiveVersionRequest ->
  UpdateTemplateActiveVersion
mkUpdateTemplateActiveVersion
  templateName
  templateType
  templateActiveVersionRequest =
    UpdateTemplateActiveVersion'
      { templateName,
        templateType,
        templateActiveVersionRequest
      }

-- | The name of the message template. A template name must start with an alphanumeric character and can contain a maximum of 128 characters. The characters can be alphanumeric characters, underscores (_), or hyphens (-). Template names are case sensitive.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utavTemplateName :: Lens.Lens' UpdateTemplateActiveVersion Core.Text
utavTemplateName = Lens.field @"templateName"
{-# DEPRECATED utavTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

-- | The type of channel that the message template is designed for. Valid values are: EMAIL, PUSH, SMS, and VOICE.
--
-- /Note:/ Consider using 'templateType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utavTemplateType :: Lens.Lens' UpdateTemplateActiveVersion Core.Text
utavTemplateType = Lens.field @"templateType"
{-# DEPRECATED utavTemplateType "Use generic-lens or generic-optics with 'templateType' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'templateActiveVersionRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utavTemplateActiveVersionRequest :: Lens.Lens' UpdateTemplateActiveVersion Types.TemplateActiveVersionRequest
utavTemplateActiveVersionRequest = Lens.field @"templateActiveVersionRequest"
{-# DEPRECATED utavTemplateActiveVersionRequest "Use generic-lens or generic-optics with 'templateActiveVersionRequest' instead." #-}

instance Core.FromJSON UpdateTemplateActiveVersion where
  toJSON UpdateTemplateActiveVersion {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "TemplateActiveVersionRequest"
                  Core..= templateActiveVersionRequest
              )
          ]
      )

instance Core.AWSRequest UpdateTemplateActiveVersion where
  type
    Rs UpdateTemplateActiveVersion =
      UpdateTemplateActiveVersionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath =
          Core.rawPath
            ( "/v1/templates/" Core.<> (Core.toText templateName) Core.<> ("/")
                Core.<> (Core.toText templateType)
                Core.<> ("/active-version")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateTemplateActiveVersionResponse'
            Core.<$> (Core.eitherParseJSON x) Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateTemplateActiveVersionResponse' smart constructor.
data UpdateTemplateActiveVersionResponse = UpdateTemplateActiveVersionResponse'
  { messageBody :: Types.MessageBody,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateTemplateActiveVersionResponse' value with any optional fields omitted.
mkUpdateTemplateActiveVersionResponse ::
  -- | 'messageBody'
  Types.MessageBody ->
  -- | 'responseStatus'
  Core.Int ->
  UpdateTemplateActiveVersionResponse
mkUpdateTemplateActiveVersionResponse messageBody responseStatus =
  UpdateTemplateActiveVersionResponse' {messageBody, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'messageBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utavrrsMessageBody :: Lens.Lens' UpdateTemplateActiveVersionResponse Types.MessageBody
utavrrsMessageBody = Lens.field @"messageBody"
{-# DEPRECATED utavrrsMessageBody "Use generic-lens or generic-optics with 'messageBody' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utavrrsResponseStatus :: Lens.Lens' UpdateTemplateActiveVersionResponse Core.Int
utavrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED utavrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.DeleteTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an email template.
--
-- You can execute this operation no more than once per second.
module Network.AWS.SES.DeleteTemplate
  ( -- * Creating a request
    DeleteTemplate (..),
    mkDeleteTemplate,

    -- ** Request lenses
    dtTemplateName,

    -- * Destructuring the response
    DeleteTemplateResponse (..),
    mkDeleteTemplateResponse,

    -- ** Response lenses
    dtrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SES.Types as Types

-- | Represents a request to delete an email template. For more information, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/send-personalized-email-api.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkDeleteTemplate' smart constructor.
newtype DeleteTemplate = DeleteTemplate'
  { -- | The name of the template to be deleted.
    templateName :: Types.TemplateName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteTemplate' value with any optional fields omitted.
mkDeleteTemplate ::
  -- | 'templateName'
  Types.TemplateName ->
  DeleteTemplate
mkDeleteTemplate templateName = DeleteTemplate' {templateName}

-- | The name of the template to be deleted.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtTemplateName :: Lens.Lens' DeleteTemplate Types.TemplateName
dtTemplateName = Lens.field @"templateName"
{-# DEPRECATED dtTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

instance Core.AWSRequest DeleteTemplate where
  type Rs DeleteTemplate = DeleteTemplateResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "DeleteTemplate")
                Core.<> (Core.pure ("Version", "2010-12-01"))
                Core.<> (Core.toQueryValue "TemplateName" templateName)
            )
      }
  response =
    Response.receiveXMLWrapper
      "DeleteTemplateResult"
      ( \s h x ->
          DeleteTemplateResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteTemplateResponse' smart constructor.
newtype DeleteTemplateResponse = DeleteTemplateResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteTemplateResponse' value with any optional fields omitted.
mkDeleteTemplateResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteTemplateResponse
mkDeleteTemplateResponse responseStatus =
  DeleteTemplateResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrrsResponseStatus :: Lens.Lens' DeleteTemplateResponse Core.Int
dtrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dtrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

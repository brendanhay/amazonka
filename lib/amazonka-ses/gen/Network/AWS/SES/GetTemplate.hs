{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.GetTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Displays the template object (which includes the Subject line, HTML part and text part) for the template you specify.
--
-- You can execute this operation no more than once per second.
module Network.AWS.SES.GetTemplate
  ( -- * Creating a request
    GetTemplate (..),
    mkGetTemplate,

    -- ** Request lenses
    gtTemplateName,

    -- * Destructuring the response
    GetTemplateResponse (..),
    mkGetTemplateResponse,

    -- ** Response lenses
    gtrrsTemplate,
    gtrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SES.Types as Types

-- | /See:/ 'mkGetTemplate' smart constructor.
newtype GetTemplate = GetTemplate'
  { -- | The name of the template you want to retrieve.
    templateName :: Types.TemplateName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetTemplate' value with any optional fields omitted.
mkGetTemplate ::
  -- | 'templateName'
  Types.TemplateName ->
  GetTemplate
mkGetTemplate templateName = GetTemplate' {templateName}

-- | The name of the template you want to retrieve.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtTemplateName :: Lens.Lens' GetTemplate Types.TemplateName
gtTemplateName = Lens.field @"templateName"
{-# DEPRECATED gtTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

instance Core.AWSRequest GetTemplate where
  type Rs GetTemplate = GetTemplateResponse
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
            ( Core.pure ("Action", "GetTemplate")
                Core.<> (Core.pure ("Version", "2010-12-01"))
                Core.<> (Core.toQueryValue "TemplateName" templateName)
            )
      }
  response =
    Response.receiveXMLWrapper
      "GetTemplateResult"
      ( \s h x ->
          GetTemplateResponse'
            Core.<$> (x Core..@? "Template") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetTemplateResponse' smart constructor.
data GetTemplateResponse = GetTemplateResponse'
  { template :: Core.Maybe Types.Template,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetTemplateResponse' value with any optional fields omitted.
mkGetTemplateResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetTemplateResponse
mkGetTemplateResponse responseStatus =
  GetTemplateResponse' {template = Core.Nothing, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'template' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtrrsTemplate :: Lens.Lens' GetTemplateResponse (Core.Maybe Types.Template)
gtrrsTemplate = Lens.field @"template"
{-# DEPRECATED gtrrsTemplate "Use generic-lens or generic-optics with 'template' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtrrsResponseStatus :: Lens.Lens' GetTemplateResponse Core.Int
gtrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gtrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

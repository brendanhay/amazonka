{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.TestRenderTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a preview of the MIME content of an email when provided with a template and a set of replacement data.
--
-- You can execute this operation no more than once per second.
module Network.AWS.SES.TestRenderTemplate
  ( -- * Creating a request
    TestRenderTemplate (..),
    mkTestRenderTemplate,

    -- ** Request lenses
    trtTemplateName,
    trtTemplateData,

    -- * Destructuring the response
    TestRenderTemplateResponse (..),
    mkTestRenderTemplateResponse,

    -- ** Response lenses
    trtrrsRenderedTemplate,
    trtrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SES.Types as Types

-- | /See:/ 'mkTestRenderTemplate' smart constructor.
data TestRenderTemplate = TestRenderTemplate'
  { -- | The name of the template that you want to render.
    templateName :: Types.TemplateName,
    -- | A list of replacement values to apply to the template. This parameter is a JSON object, typically consisting of key-value pairs in which the keys correspond to replacement tags in the email template.
    templateData :: Types.TemplateData
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TestRenderTemplate' value with any optional fields omitted.
mkTestRenderTemplate ::
  -- | 'templateName'
  Types.TemplateName ->
  -- | 'templateData'
  Types.TemplateData ->
  TestRenderTemplate
mkTestRenderTemplate templateName templateData =
  TestRenderTemplate' {templateName, templateData}

-- | The name of the template that you want to render.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trtTemplateName :: Lens.Lens' TestRenderTemplate Types.TemplateName
trtTemplateName = Lens.field @"templateName"
{-# DEPRECATED trtTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

-- | A list of replacement values to apply to the template. This parameter is a JSON object, typically consisting of key-value pairs in which the keys correspond to replacement tags in the email template.
--
-- /Note:/ Consider using 'templateData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trtTemplateData :: Lens.Lens' TestRenderTemplate Types.TemplateData
trtTemplateData = Lens.field @"templateData"
{-# DEPRECATED trtTemplateData "Use generic-lens or generic-optics with 'templateData' instead." #-}

instance Core.AWSRequest TestRenderTemplate where
  type Rs TestRenderTemplate = TestRenderTemplateResponse
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
            ( Core.pure ("Action", "TestRenderTemplate")
                Core.<> (Core.pure ("Version", "2010-12-01"))
                Core.<> (Core.toQueryValue "TemplateName" templateName)
                Core.<> (Core.toQueryValue "TemplateData" templateData)
            )
      }
  response =
    Response.receiveXMLWrapper
      "TestRenderTemplateResult"
      ( \s h x ->
          TestRenderTemplateResponse'
            Core.<$> (x Core..@? "RenderedTemplate")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkTestRenderTemplateResponse' smart constructor.
data TestRenderTemplateResponse = TestRenderTemplateResponse'
  { -- | The complete MIME message rendered by applying the data in the TemplateData parameter to the template specified in the TemplateName parameter.
    renderedTemplate :: Core.Maybe Types.RenderedTemplate,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TestRenderTemplateResponse' value with any optional fields omitted.
mkTestRenderTemplateResponse ::
  -- | 'responseStatus'
  Core.Int ->
  TestRenderTemplateResponse
mkTestRenderTemplateResponse responseStatus =
  TestRenderTemplateResponse'
    { renderedTemplate = Core.Nothing,
      responseStatus
    }

-- | The complete MIME message rendered by applying the data in the TemplateData parameter to the template specified in the TemplateName parameter.
--
-- /Note:/ Consider using 'renderedTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trtrrsRenderedTemplate :: Lens.Lens' TestRenderTemplateResponse (Core.Maybe Types.RenderedTemplate)
trtrrsRenderedTemplate = Lens.field @"renderedTemplate"
{-# DEPRECATED trtrrsRenderedTemplate "Use generic-lens or generic-optics with 'renderedTemplate' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trtrrsResponseStatus :: Lens.Lens' TestRenderTemplateResponse Core.Int
trtrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED trtrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

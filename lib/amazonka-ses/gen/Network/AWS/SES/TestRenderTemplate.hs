{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      TestRenderTemplate (..)
    , mkTestRenderTemplate
    -- ** Request lenses
    , trtTemplateName
    , trtTemplateData

    -- * Destructuring the response
    , TestRenderTemplateResponse (..)
    , mkTestRenderTemplateResponse
    -- ** Response lenses
    , trtrrsRenderedTemplate
    , trtrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SES.Types as Types

-- | /See:/ 'mkTestRenderTemplate' smart constructor.
data TestRenderTemplate = TestRenderTemplate'
  { templateName :: Types.TemplateName
    -- ^ The name of the template that you want to render.
  , templateData :: Types.TemplateData
    -- ^ A list of replacement values to apply to the template. This parameter is a JSON object, typically consisting of key-value pairs in which the keys correspond to replacement tags in the email template.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TestRenderTemplate' value with any optional fields omitted.
mkTestRenderTemplate
    :: Types.TemplateName -- ^ 'templateName'
    -> Types.TemplateData -- ^ 'templateData'
    -> TestRenderTemplate
mkTestRenderTemplate templateName templateData
  = TestRenderTemplate'{templateName, templateData}

-- | The name of the template that you want to render.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trtTemplateName :: Lens.Lens' TestRenderTemplate Types.TemplateName
trtTemplateName = Lens.field @"templateName"
{-# INLINEABLE trtTemplateName #-}
{-# DEPRECATED templateName "Use generic-lens or generic-optics with 'templateName' instead"  #-}

-- | A list of replacement values to apply to the template. This parameter is a JSON object, typically consisting of key-value pairs in which the keys correspond to replacement tags in the email template.
--
-- /Note:/ Consider using 'templateData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trtTemplateData :: Lens.Lens' TestRenderTemplate Types.TemplateData
trtTemplateData = Lens.field @"templateData"
{-# INLINEABLE trtTemplateData #-}
{-# DEPRECATED templateData "Use generic-lens or generic-optics with 'templateData' instead"  #-}

instance Core.ToQuery TestRenderTemplate where
        toQuery TestRenderTemplate{..}
          = Core.toQueryPair "Action" ("TestRenderTemplate" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-12-01" :: Core.Text)
              Core.<> Core.toQueryPair "TemplateName" templateName
              Core.<> Core.toQueryPair "TemplateData" templateData

instance Core.ToHeaders TestRenderTemplate where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest TestRenderTemplate where
        type Rs TestRenderTemplate = TestRenderTemplateResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "TestRenderTemplateResult"
              (\ s h x ->
                 TestRenderTemplateResponse' Core.<$>
                   (x Core..@? "RenderedTemplate") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkTestRenderTemplateResponse' smart constructor.
data TestRenderTemplateResponse = TestRenderTemplateResponse'
  { renderedTemplate :: Core.Maybe Types.RenderedTemplate
    -- ^ The complete MIME message rendered by applying the data in the TemplateData parameter to the template specified in the TemplateName parameter.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TestRenderTemplateResponse' value with any optional fields omitted.
mkTestRenderTemplateResponse
    :: Core.Int -- ^ 'responseStatus'
    -> TestRenderTemplateResponse
mkTestRenderTemplateResponse responseStatus
  = TestRenderTemplateResponse'{renderedTemplate = Core.Nothing,
                                responseStatus}

-- | The complete MIME message rendered by applying the data in the TemplateData parameter to the template specified in the TemplateName parameter.
--
-- /Note:/ Consider using 'renderedTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trtrrsRenderedTemplate :: Lens.Lens' TestRenderTemplateResponse (Core.Maybe Types.RenderedTemplate)
trtrrsRenderedTemplate = Lens.field @"renderedTemplate"
{-# INLINEABLE trtrrsRenderedTemplate #-}
{-# DEPRECATED renderedTemplate "Use generic-lens or generic-optics with 'renderedTemplate' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trtrrsResponseStatus :: Lens.Lens' TestRenderTemplateResponse Core.Int
trtrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE trtrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

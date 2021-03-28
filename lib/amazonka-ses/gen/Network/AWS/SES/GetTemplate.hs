{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      GetTemplate (..)
    , mkGetTemplate
    -- ** Request lenses
    , gtTemplateName

    -- * Destructuring the response
    , GetTemplateResponse (..)
    , mkGetTemplateResponse
    -- ** Response lenses
    , gtrrsTemplate
    , gtrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SES.Types as Types

-- | /See:/ 'mkGetTemplate' smart constructor.
newtype GetTemplate = GetTemplate'
  { templateName :: Types.TemplateName
    -- ^ The name of the template you want to retrieve.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetTemplate' value with any optional fields omitted.
mkGetTemplate
    :: Types.TemplateName -- ^ 'templateName'
    -> GetTemplate
mkGetTemplate templateName = GetTemplate'{templateName}

-- | The name of the template you want to retrieve.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtTemplateName :: Lens.Lens' GetTemplate Types.TemplateName
gtTemplateName = Lens.field @"templateName"
{-# INLINEABLE gtTemplateName #-}
{-# DEPRECATED templateName "Use generic-lens or generic-optics with 'templateName' instead"  #-}

instance Core.ToQuery GetTemplate where
        toQuery GetTemplate{..}
          = Core.toQueryPair "Action" ("GetTemplate" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2010-12-01" :: Core.Text)
              Core.<> Core.toQueryPair "TemplateName" templateName

instance Core.ToHeaders GetTemplate where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest GetTemplate where
        type Rs GetTemplate = GetTemplateResponse
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
          = Response.receiveXMLWrapper "GetTemplateResult"
              (\ s h x ->
                 GetTemplateResponse' Core.<$>
                   (x Core..@? "Template") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetTemplateResponse' smart constructor.
data GetTemplateResponse = GetTemplateResponse'
  { template :: Core.Maybe Types.Template
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetTemplateResponse' value with any optional fields omitted.
mkGetTemplateResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetTemplateResponse
mkGetTemplateResponse responseStatus
  = GetTemplateResponse'{template = Core.Nothing, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'template' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtrrsTemplate :: Lens.Lens' GetTemplateResponse (Core.Maybe Types.Template)
gtrrsTemplate = Lens.field @"template"
{-# INLINEABLE gtrrsTemplate #-}
{-# DEPRECATED template "Use generic-lens or generic-optics with 'template' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtrrsResponseStatus :: Lens.Lens' GetTemplateResponse Core.Int
gtrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gtrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

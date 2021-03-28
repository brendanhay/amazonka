{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.CreateTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an email template. Email templates enable you to send personalized email to one or more destinations in a single API operation. For more information, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/send-personalized-email-api.html Amazon SES Developer Guide> .
--
-- You can execute this operation no more than once per second.
module Network.AWS.SES.CreateTemplate
    (
    -- * Creating a request
      CreateTemplate (..)
    , mkCreateTemplate
    -- ** Request lenses
    , ctTemplate

    -- * Destructuring the response
    , CreateTemplateResponse (..)
    , mkCreateTemplateResponse
    -- ** Response lenses
    , ctrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SES.Types as Types

-- | Represents a request to create an email template. For more information, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/send-personalized-email-api.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkCreateTemplate' smart constructor.
newtype CreateTemplate = CreateTemplate'
  { template :: Types.Template
    -- ^ The content of the email, composed of a subject line, an HTML part, and a text-only part.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CreateTemplate' value with any optional fields omitted.
mkCreateTemplate
    :: Types.Template -- ^ 'template'
    -> CreateTemplate
mkCreateTemplate template = CreateTemplate'{template}

-- | The content of the email, composed of a subject line, an HTML part, and a text-only part.
--
-- /Note:/ Consider using 'template' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctTemplate :: Lens.Lens' CreateTemplate Types.Template
ctTemplate = Lens.field @"template"
{-# INLINEABLE ctTemplate #-}
{-# DEPRECATED template "Use generic-lens or generic-optics with 'template' instead"  #-}

instance Core.ToQuery CreateTemplate where
        toQuery CreateTemplate{..}
          = Core.toQueryPair "Action" ("CreateTemplate" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2010-12-01" :: Core.Text)
              Core.<> Core.toQueryPair "Template" template

instance Core.ToHeaders CreateTemplate where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateTemplate where
        type Rs CreateTemplate = CreateTemplateResponse
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
          = Response.receiveXMLWrapper "CreateTemplateResult"
              (\ s h x ->
                 CreateTemplateResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateTemplateResponse' smart constructor.
newtype CreateTemplateResponse = CreateTemplateResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CreateTemplateResponse' value with any optional fields omitted.
mkCreateTemplateResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateTemplateResponse
mkCreateTemplateResponse responseStatus
  = CreateTemplateResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctrrsResponseStatus :: Lens.Lens' CreateTemplateResponse Core.Int
ctrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ctrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

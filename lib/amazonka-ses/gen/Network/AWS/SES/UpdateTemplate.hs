{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.UpdateTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an email template. Email templates enable you to send personalized email to one or more destinations in a single API operation. For more information, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/send-personalized-email-api.html Amazon SES Developer Guide> .
--
-- You can execute this operation no more than once per second.
module Network.AWS.SES.UpdateTemplate
    (
    -- * Creating a request
      UpdateTemplate (..)
    , mkUpdateTemplate
    -- ** Request lenses
    , utTemplate

    -- * Destructuring the response
    , UpdateTemplateResponse (..)
    , mkUpdateTemplateResponse
    -- ** Response lenses
    , utrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SES.Types as Types

-- | /See:/ 'mkUpdateTemplate' smart constructor.
newtype UpdateTemplate = UpdateTemplate'
  { template :: Types.Template
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateTemplate' value with any optional fields omitted.
mkUpdateTemplate
    :: Types.Template -- ^ 'template'
    -> UpdateTemplate
mkUpdateTemplate template = UpdateTemplate'{template}

-- | Undocumented field.
--
-- /Note:/ Consider using 'template' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utTemplate :: Lens.Lens' UpdateTemplate Types.Template
utTemplate = Lens.field @"template"
{-# INLINEABLE utTemplate #-}
{-# DEPRECATED template "Use generic-lens or generic-optics with 'template' instead"  #-}

instance Core.ToQuery UpdateTemplate where
        toQuery UpdateTemplate{..}
          = Core.toQueryPair "Action" ("UpdateTemplate" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2010-12-01" :: Core.Text)
              Core.<> Core.toQueryPair "Template" template

instance Core.ToHeaders UpdateTemplate where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest UpdateTemplate where
        type Rs UpdateTemplate = UpdateTemplateResponse
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
          = Response.receiveXMLWrapper "UpdateTemplateResult"
              (\ s h x ->
                 UpdateTemplateResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateTemplateResponse' smart constructor.
newtype UpdateTemplateResponse = UpdateTemplateResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateTemplateResponse' value with any optional fields omitted.
mkUpdateTemplateResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateTemplateResponse
mkUpdateTemplateResponse responseStatus
  = UpdateTemplateResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utrrsResponseStatus :: Lens.Lens' UpdateTemplateResponse Core.Int
utrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE utrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

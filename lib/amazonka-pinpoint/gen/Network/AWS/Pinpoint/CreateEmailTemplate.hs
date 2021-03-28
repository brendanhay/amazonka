{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.CreateEmailTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a message template for messages that are sent through the email channel.
module Network.AWS.Pinpoint.CreateEmailTemplate
    (
    -- * Creating a request
      CreateEmailTemplate (..)
    , mkCreateEmailTemplate
    -- ** Request lenses
    , cetTemplateName
    , cetEmailTemplateRequest

    -- * Destructuring the response
    , CreateEmailTemplateResponse (..)
    , mkCreateEmailTemplateResponse
    -- ** Response lenses
    , cetrrsCreateTemplateMessageBody
    , cetrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateEmailTemplate' smart constructor.
data CreateEmailTemplate = CreateEmailTemplate'
  { templateName :: Core.Text
    -- ^ The name of the message template. A template name must start with an alphanumeric character and can contain a maximum of 128 characters. The characters can be alphanumeric characters, underscores (_), or hyphens (-). Template names are case sensitive.
  , emailTemplateRequest :: Types.EmailTemplateRequest
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateEmailTemplate' value with any optional fields omitted.
mkCreateEmailTemplate
    :: Core.Text -- ^ 'templateName'
    -> Types.EmailTemplateRequest -- ^ 'emailTemplateRequest'
    -> CreateEmailTemplate
mkCreateEmailTemplate templateName emailTemplateRequest
  = CreateEmailTemplate'{templateName, emailTemplateRequest}

-- | The name of the message template. A template name must start with an alphanumeric character and can contain a maximum of 128 characters. The characters can be alphanumeric characters, underscores (_), or hyphens (-). Template names are case sensitive.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cetTemplateName :: Lens.Lens' CreateEmailTemplate Core.Text
cetTemplateName = Lens.field @"templateName"
{-# INLINEABLE cetTemplateName #-}
{-# DEPRECATED templateName "Use generic-lens or generic-optics with 'templateName' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'emailTemplateRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cetEmailTemplateRequest :: Lens.Lens' CreateEmailTemplate Types.EmailTemplateRequest
cetEmailTemplateRequest = Lens.field @"emailTemplateRequest"
{-# INLINEABLE cetEmailTemplateRequest #-}
{-# DEPRECATED emailTemplateRequest "Use generic-lens or generic-optics with 'emailTemplateRequest' instead"  #-}

instance Core.ToQuery CreateEmailTemplate where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateEmailTemplate where
        toHeaders CreateEmailTemplate{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateEmailTemplate where
        toJSON CreateEmailTemplate{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("EmailTemplateRequest" Core..= emailTemplateRequest)])

instance Core.AWSRequest CreateEmailTemplate where
        type Rs CreateEmailTemplate = CreateEmailTemplateResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/v1/templates/" Core.<> Core.toText templateName Core.<> "/email",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateEmailTemplateResponse' Core.<$>
                   (Core.eitherParseJSON x) Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateEmailTemplateResponse' smart constructor.
data CreateEmailTemplateResponse = CreateEmailTemplateResponse'
  { createTemplateMessageBody :: Types.CreateTemplateMessageBody
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateEmailTemplateResponse' value with any optional fields omitted.
mkCreateEmailTemplateResponse
    :: Types.CreateTemplateMessageBody -- ^ 'createTemplateMessageBody'
    -> Core.Int -- ^ 'responseStatus'
    -> CreateEmailTemplateResponse
mkCreateEmailTemplateResponse createTemplateMessageBody
  responseStatus
  = CreateEmailTemplateResponse'{createTemplateMessageBody,
                                 responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'createTemplateMessageBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cetrrsCreateTemplateMessageBody :: Lens.Lens' CreateEmailTemplateResponse Types.CreateTemplateMessageBody
cetrrsCreateTemplateMessageBody = Lens.field @"createTemplateMessageBody"
{-# INLINEABLE cetrrsCreateTemplateMessageBody #-}
{-# DEPRECATED createTemplateMessageBody "Use generic-lens or generic-optics with 'createTemplateMessageBody' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cetrrsResponseStatus :: Lens.Lens' CreateEmailTemplateResponse Core.Int
cetrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cetrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

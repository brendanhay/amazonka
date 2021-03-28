{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.CreatePushTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a message template for messages that are sent through a push notification channel.
module Network.AWS.Pinpoint.CreatePushTemplate
    (
    -- * Creating a request
      CreatePushTemplate (..)
    , mkCreatePushTemplate
    -- ** Request lenses
    , cptTemplateName
    , cptPushNotificationTemplateRequest

    -- * Destructuring the response
    , CreatePushTemplateResponse (..)
    , mkCreatePushTemplateResponse
    -- ** Response lenses
    , cptrrsCreateTemplateMessageBody
    , cptrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreatePushTemplate' smart constructor.
data CreatePushTemplate = CreatePushTemplate'
  { templateName :: Core.Text
    -- ^ The name of the message template. A template name must start with an alphanumeric character and can contain a maximum of 128 characters. The characters can be alphanumeric characters, underscores (_), or hyphens (-). Template names are case sensitive.
  , pushNotificationTemplateRequest :: Types.PushNotificationTemplateRequest
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreatePushTemplate' value with any optional fields omitted.
mkCreatePushTemplate
    :: Core.Text -- ^ 'templateName'
    -> Types.PushNotificationTemplateRequest -- ^ 'pushNotificationTemplateRequest'
    -> CreatePushTemplate
mkCreatePushTemplate templateName pushNotificationTemplateRequest
  = CreatePushTemplate'{templateName,
                        pushNotificationTemplateRequest}

-- | The name of the message template. A template name must start with an alphanumeric character and can contain a maximum of 128 characters. The characters can be alphanumeric characters, underscores (_), or hyphens (-). Template names are case sensitive.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cptTemplateName :: Lens.Lens' CreatePushTemplate Core.Text
cptTemplateName = Lens.field @"templateName"
{-# INLINEABLE cptTemplateName #-}
{-# DEPRECATED templateName "Use generic-lens or generic-optics with 'templateName' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'pushNotificationTemplateRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cptPushNotificationTemplateRequest :: Lens.Lens' CreatePushTemplate Types.PushNotificationTemplateRequest
cptPushNotificationTemplateRequest = Lens.field @"pushNotificationTemplateRequest"
{-# INLINEABLE cptPushNotificationTemplateRequest #-}
{-# DEPRECATED pushNotificationTemplateRequest "Use generic-lens or generic-optics with 'pushNotificationTemplateRequest' instead"  #-}

instance Core.ToQuery CreatePushTemplate where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreatePushTemplate where
        toHeaders CreatePushTemplate{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreatePushTemplate where
        toJSON CreatePushTemplate{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("PushNotificationTemplateRequest" Core..=
                       pushNotificationTemplateRequest)])

instance Core.AWSRequest CreatePushTemplate where
        type Rs CreatePushTemplate = CreatePushTemplateResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/v1/templates/" Core.<> Core.toText templateName Core.<> "/push",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreatePushTemplateResponse' Core.<$>
                   (Core.eitherParseJSON x) Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreatePushTemplateResponse' smart constructor.
data CreatePushTemplateResponse = CreatePushTemplateResponse'
  { createTemplateMessageBody :: Types.CreateTemplateMessageBody
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreatePushTemplateResponse' value with any optional fields omitted.
mkCreatePushTemplateResponse
    :: Types.CreateTemplateMessageBody -- ^ 'createTemplateMessageBody'
    -> Core.Int -- ^ 'responseStatus'
    -> CreatePushTemplateResponse
mkCreatePushTemplateResponse createTemplateMessageBody
  responseStatus
  = CreatePushTemplateResponse'{createTemplateMessageBody,
                                responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'createTemplateMessageBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cptrrsCreateTemplateMessageBody :: Lens.Lens' CreatePushTemplateResponse Types.CreateTemplateMessageBody
cptrrsCreateTemplateMessageBody = Lens.field @"createTemplateMessageBody"
{-# INLINEABLE cptrrsCreateTemplateMessageBody #-}
{-# DEPRECATED createTemplateMessageBody "Use generic-lens or generic-optics with 'createTemplateMessageBody' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cptrrsResponseStatus :: Lens.Lens' CreatePushTemplateResponse Core.Int
cptrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cptrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

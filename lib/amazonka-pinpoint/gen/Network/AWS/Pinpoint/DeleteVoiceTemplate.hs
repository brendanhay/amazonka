{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.DeleteVoiceTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a message template for messages that were sent through the voice channel.
module Network.AWS.Pinpoint.DeleteVoiceTemplate
    (
    -- * Creating a request
      DeleteVoiceTemplate (..)
    , mkDeleteVoiceTemplate
    -- ** Request lenses
    , dvtTemplateName
    , dvtVersion

    -- * Destructuring the response
    , DeleteVoiceTemplateResponse (..)
    , mkDeleteVoiceTemplateResponse
    -- ** Response lenses
    , dvtrrsMessageBody
    , dvtrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteVoiceTemplate' smart constructor.
data DeleteVoiceTemplate = DeleteVoiceTemplate'
  { templateName :: Core.Text
    -- ^ The name of the message template. A template name must start with an alphanumeric character and can contain a maximum of 128 characters. The characters can be alphanumeric characters, underscores (_), or hyphens (-). Template names are case sensitive.
  , version :: Core.Maybe Core.Text
    -- ^ The unique identifier for the version of the message template to update, retrieve information about, or delete. To retrieve identifiers and other information for all the versions of a template, use the <link>Template Versions resource.
--
-- If specified, this value must match the identifier for an existing template version. If specified for an update operation, this value must match the identifier for the latest existing version of the template. This restriction helps ensure that race conditions don't occur.
-- If you don't specify a value for this parameter, Amazon Pinpoint does the following:
--
--     * For a get operation, retrieves information about the active version of the template.
--
--
--     * For an update operation, saves the updates to (overwrites) the latest existing version of the template, if the create-new-version parameter isn't used or is set to false.
--
--
--     * For a delete operation, deletes the template, including all versions of the template.
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteVoiceTemplate' value with any optional fields omitted.
mkDeleteVoiceTemplate
    :: Core.Text -- ^ 'templateName'
    -> DeleteVoiceTemplate
mkDeleteVoiceTemplate templateName
  = DeleteVoiceTemplate'{templateName, version = Core.Nothing}

-- | The name of the message template. A template name must start with an alphanumeric character and can contain a maximum of 128 characters. The characters can be alphanumeric characters, underscores (_), or hyphens (-). Template names are case sensitive.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvtTemplateName :: Lens.Lens' DeleteVoiceTemplate Core.Text
dvtTemplateName = Lens.field @"templateName"
{-# INLINEABLE dvtTemplateName #-}
{-# DEPRECATED templateName "Use generic-lens or generic-optics with 'templateName' instead"  #-}

-- | The unique identifier for the version of the message template to update, retrieve information about, or delete. To retrieve identifiers and other information for all the versions of a template, use the <link>Template Versions resource.
--
-- If specified, this value must match the identifier for an existing template version. If specified for an update operation, this value must match the identifier for the latest existing version of the template. This restriction helps ensure that race conditions don't occur.
-- If you don't specify a value for this parameter, Amazon Pinpoint does the following:
--
--     * For a get operation, retrieves information about the active version of the template.
--
--
--     * For an update operation, saves the updates to (overwrites) the latest existing version of the template, if the create-new-version parameter isn't used or is set to false.
--
--
--     * For a delete operation, deletes the template, including all versions of the template.
--
--
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvtVersion :: Lens.Lens' DeleteVoiceTemplate (Core.Maybe Core.Text)
dvtVersion = Lens.field @"version"
{-# INLINEABLE dvtVersion #-}
{-# DEPRECATED version "Use generic-lens or generic-optics with 'version' instead"  #-}

instance Core.ToQuery DeleteVoiceTemplate where
        toQuery DeleteVoiceTemplate{..}
          = Core.maybe Core.mempty (Core.toQueryPair "version") version

instance Core.ToHeaders DeleteVoiceTemplate where
        toHeaders DeleteVoiceTemplate{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest DeleteVoiceTemplate where
        type Rs DeleteVoiceTemplate = DeleteVoiceTemplateResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath =
                           "/v1/templates/" Core.<> Core.toText templateName Core.<> "/voice",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteVoiceTemplateResponse' Core.<$>
                   (Core.eitherParseJSON x) Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteVoiceTemplateResponse' smart constructor.
data DeleteVoiceTemplateResponse = DeleteVoiceTemplateResponse'
  { messageBody :: Types.MessageBody
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteVoiceTemplateResponse' value with any optional fields omitted.
mkDeleteVoiceTemplateResponse
    :: Types.MessageBody -- ^ 'messageBody'
    -> Core.Int -- ^ 'responseStatus'
    -> DeleteVoiceTemplateResponse
mkDeleteVoiceTemplateResponse messageBody responseStatus
  = DeleteVoiceTemplateResponse'{messageBody, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'messageBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvtrrsMessageBody :: Lens.Lens' DeleteVoiceTemplateResponse Types.MessageBody
dvtrrsMessageBody = Lens.field @"messageBody"
{-# INLINEABLE dvtrrsMessageBody #-}
{-# DEPRECATED messageBody "Use generic-lens or generic-optics with 'messageBody' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvtrrsResponseStatus :: Lens.Lens' DeleteVoiceTemplateResponse Core.Int
dvtrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dvtrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.DeleteEmailTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a message template for messages that were sent through the email channel.
module Network.AWS.Pinpoint.DeleteEmailTemplate
    (
    -- * Creating a request
      DeleteEmailTemplate (..)
    , mkDeleteEmailTemplate
    -- ** Request lenses
    , detTemplateName
    , detVersion

    -- * Destructuring the response
    , DeleteEmailTemplateResponse (..)
    , mkDeleteEmailTemplateResponse
    -- ** Response lenses
    , detrrsMessageBody
    , detrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteEmailTemplate' smart constructor.
data DeleteEmailTemplate = DeleteEmailTemplate'
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

-- | Creates a 'DeleteEmailTemplate' value with any optional fields omitted.
mkDeleteEmailTemplate
    :: Core.Text -- ^ 'templateName'
    -> DeleteEmailTemplate
mkDeleteEmailTemplate templateName
  = DeleteEmailTemplate'{templateName, version = Core.Nothing}

-- | The name of the message template. A template name must start with an alphanumeric character and can contain a maximum of 128 characters. The characters can be alphanumeric characters, underscores (_), or hyphens (-). Template names are case sensitive.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detTemplateName :: Lens.Lens' DeleteEmailTemplate Core.Text
detTemplateName = Lens.field @"templateName"
{-# INLINEABLE detTemplateName #-}
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
detVersion :: Lens.Lens' DeleteEmailTemplate (Core.Maybe Core.Text)
detVersion = Lens.field @"version"
{-# INLINEABLE detVersion #-}
{-# DEPRECATED version "Use generic-lens or generic-optics with 'version' instead"  #-}

instance Core.ToQuery DeleteEmailTemplate where
        toQuery DeleteEmailTemplate{..}
          = Core.maybe Core.mempty (Core.toQueryPair "version") version

instance Core.ToHeaders DeleteEmailTemplate where
        toHeaders DeleteEmailTemplate{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest DeleteEmailTemplate where
        type Rs DeleteEmailTemplate = DeleteEmailTemplateResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath =
                           "/v1/templates/" Core.<> Core.toText templateName Core.<> "/email",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteEmailTemplateResponse' Core.<$>
                   (Core.eitherParseJSON x) Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteEmailTemplateResponse' smart constructor.
data DeleteEmailTemplateResponse = DeleteEmailTemplateResponse'
  { messageBody :: Types.MessageBody
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteEmailTemplateResponse' value with any optional fields omitted.
mkDeleteEmailTemplateResponse
    :: Types.MessageBody -- ^ 'messageBody'
    -> Core.Int -- ^ 'responseStatus'
    -> DeleteEmailTemplateResponse
mkDeleteEmailTemplateResponse messageBody responseStatus
  = DeleteEmailTemplateResponse'{messageBody, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'messageBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detrrsMessageBody :: Lens.Lens' DeleteEmailTemplateResponse Types.MessageBody
detrrsMessageBody = Lens.field @"messageBody"
{-# INLINEABLE detrrsMessageBody #-}
{-# DEPRECATED messageBody "Use generic-lens or generic-optics with 'messageBody' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detrrsResponseStatus :: Lens.Lens' DeleteEmailTemplateResponse Core.Int
detrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE detrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

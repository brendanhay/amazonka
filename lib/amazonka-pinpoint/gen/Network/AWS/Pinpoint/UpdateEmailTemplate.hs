{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.UpdateEmailTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing message template for messages that are sent through the email channel.
module Network.AWS.Pinpoint.UpdateEmailTemplate
    (
    -- * Creating a request
      UpdateEmailTemplate (..)
    , mkUpdateEmailTemplate
    -- ** Request lenses
    , uetTemplateName
    , uetEmailTemplateRequest
    , uetCreateNewVersion
    , uetVersion

    -- * Destructuring the response
    , UpdateEmailTemplateResponse (..)
    , mkUpdateEmailTemplateResponse
    -- ** Response lenses
    , uetrrsMessageBody
    , uetrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateEmailTemplate' smart constructor.
data UpdateEmailTemplate = UpdateEmailTemplate'
  { templateName :: Core.Text
    -- ^ The name of the message template. A template name must start with an alphanumeric character and can contain a maximum of 128 characters. The characters can be alphanumeric characters, underscores (_), or hyphens (-). Template names are case sensitive.
  , emailTemplateRequest :: Types.EmailTemplateRequest
  , createNewVersion :: Core.Maybe Core.Bool
    -- ^ Specifies whether to save the updates as a new version of the message template. Valid values are: true, save the updates as a new version; and, false, save the updates to (overwrite) the latest existing version of the template.
--
-- If you don't specify a value for this parameter, Amazon Pinpoint saves the updates to (overwrites) the latest existing version of the template. If you specify a value of true for this parameter, don't specify a value for the version parameter. Otherwise, an error will occur.
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

-- | Creates a 'UpdateEmailTemplate' value with any optional fields omitted.
mkUpdateEmailTemplate
    :: Core.Text -- ^ 'templateName'
    -> Types.EmailTemplateRequest -- ^ 'emailTemplateRequest'
    -> UpdateEmailTemplate
mkUpdateEmailTemplate templateName emailTemplateRequest
  = UpdateEmailTemplate'{templateName, emailTemplateRequest,
                         createNewVersion = Core.Nothing, version = Core.Nothing}

-- | The name of the message template. A template name must start with an alphanumeric character and can contain a maximum of 128 characters. The characters can be alphanumeric characters, underscores (_), or hyphens (-). Template names are case sensitive.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uetTemplateName :: Lens.Lens' UpdateEmailTemplate Core.Text
uetTemplateName = Lens.field @"templateName"
{-# INLINEABLE uetTemplateName #-}
{-# DEPRECATED templateName "Use generic-lens or generic-optics with 'templateName' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'emailTemplateRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uetEmailTemplateRequest :: Lens.Lens' UpdateEmailTemplate Types.EmailTemplateRequest
uetEmailTemplateRequest = Lens.field @"emailTemplateRequest"
{-# INLINEABLE uetEmailTemplateRequest #-}
{-# DEPRECATED emailTemplateRequest "Use generic-lens or generic-optics with 'emailTemplateRequest' instead"  #-}

-- | Specifies whether to save the updates as a new version of the message template. Valid values are: true, save the updates as a new version; and, false, save the updates to (overwrite) the latest existing version of the template.
--
-- If you don't specify a value for this parameter, Amazon Pinpoint saves the updates to (overwrites) the latest existing version of the template. If you specify a value of true for this parameter, don't specify a value for the version parameter. Otherwise, an error will occur.
--
-- /Note:/ Consider using 'createNewVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uetCreateNewVersion :: Lens.Lens' UpdateEmailTemplate (Core.Maybe Core.Bool)
uetCreateNewVersion = Lens.field @"createNewVersion"
{-# INLINEABLE uetCreateNewVersion #-}
{-# DEPRECATED createNewVersion "Use generic-lens or generic-optics with 'createNewVersion' instead"  #-}

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
uetVersion :: Lens.Lens' UpdateEmailTemplate (Core.Maybe Core.Text)
uetVersion = Lens.field @"version"
{-# INLINEABLE uetVersion #-}
{-# DEPRECATED version "Use generic-lens or generic-optics with 'version' instead"  #-}

instance Core.ToQuery UpdateEmailTemplate where
        toQuery UpdateEmailTemplate{..}
          = Core.maybe Core.mempty (Core.toQueryPair "create-new-version")
              createNewVersion
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "version") version

instance Core.ToHeaders UpdateEmailTemplate where
        toHeaders UpdateEmailTemplate{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateEmailTemplate where
        toJSON UpdateEmailTemplate{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("EmailTemplateRequest" Core..= emailTemplateRequest)])

instance Core.AWSRequest UpdateEmailTemplate where
        type Rs UpdateEmailTemplate = UpdateEmailTemplateResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath =
                           "/v1/templates/" Core.<> Core.toText templateName Core.<> "/email",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateEmailTemplateResponse' Core.<$>
                   (Core.eitherParseJSON x) Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateEmailTemplateResponse' smart constructor.
data UpdateEmailTemplateResponse = UpdateEmailTemplateResponse'
  { messageBody :: Types.MessageBody
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateEmailTemplateResponse' value with any optional fields omitted.
mkUpdateEmailTemplateResponse
    :: Types.MessageBody -- ^ 'messageBody'
    -> Core.Int -- ^ 'responseStatus'
    -> UpdateEmailTemplateResponse
mkUpdateEmailTemplateResponse messageBody responseStatus
  = UpdateEmailTemplateResponse'{messageBody, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'messageBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uetrrsMessageBody :: Lens.Lens' UpdateEmailTemplateResponse Types.MessageBody
uetrrsMessageBody = Lens.field @"messageBody"
{-# INLINEABLE uetrrsMessageBody #-}
{-# DEPRECATED messageBody "Use generic-lens or generic-optics with 'messageBody' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uetrrsResponseStatus :: Lens.Lens' UpdateEmailTemplateResponse Core.Int
uetrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE uetrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

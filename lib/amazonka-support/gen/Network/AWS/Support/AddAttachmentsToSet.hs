{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Support.AddAttachmentsToSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds one or more attachments to an attachment set. 
--
-- An attachment set is a temporary container for attachments that you add to a case or case communication. The set is available for 1 hour after it's created. The @expiryTime@ returned in the response is when the set expires. 
module Network.AWS.Support.AddAttachmentsToSet
    (
    -- * Creating a request
      AddAttachmentsToSet (..)
    , mkAddAttachmentsToSet
    -- ** Request lenses
    , aatsAttachments
    , aatsAttachmentSetId

    -- * Destructuring the response
    , AddAttachmentsToSetResponse (..)
    , mkAddAttachmentsToSetResponse
    -- ** Response lenses
    , aatsrrsAttachmentSetId
    , aatsrrsExpiryTime
    , aatsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Support.Types as Types

-- | /See:/ 'mkAddAttachmentsToSet' smart constructor.
data AddAttachmentsToSet = AddAttachmentsToSet'
  { attachments :: [Types.Attachment]
    -- ^ One or more attachments to add to the set. You can add up to three attachments per set. The size limit is 5 MB per attachment.
--
-- In the @Attachment@ object, use the @data@ parameter to specify the contents of the attachment file. In the previous request syntax, the value for @data@ appear as @blob@ , which is represented as a base64-encoded string. The value for @fileName@ is the name of the attachment, such as @troubleshoot-screenshot.png@ .
  , attachmentSetId :: Core.Maybe Types.AttachmentSetId
    -- ^ The ID of the attachment set. If an @attachmentSetId@ is not specified, a new attachment set is created, and the ID of the set is returned in the response. If an @attachmentSetId@ is specified, the attachments are added to the specified set, if it exists.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AddAttachmentsToSet' value with any optional fields omitted.
mkAddAttachmentsToSet
    :: AddAttachmentsToSet
mkAddAttachmentsToSet
  = AddAttachmentsToSet'{attachments = Core.mempty,
                         attachmentSetId = Core.Nothing}

-- | One or more attachments to add to the set. You can add up to three attachments per set. The size limit is 5 MB per attachment.
--
-- In the @Attachment@ object, use the @data@ parameter to specify the contents of the attachment file. In the previous request syntax, the value for @data@ appear as @blob@ , which is represented as a base64-encoded string. The value for @fileName@ is the name of the attachment, such as @troubleshoot-screenshot.png@ .
--
-- /Note:/ Consider using 'attachments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aatsAttachments :: Lens.Lens' AddAttachmentsToSet [Types.Attachment]
aatsAttachments = Lens.field @"attachments"
{-# INLINEABLE aatsAttachments #-}
{-# DEPRECATED attachments "Use generic-lens or generic-optics with 'attachments' instead"  #-}

-- | The ID of the attachment set. If an @attachmentSetId@ is not specified, a new attachment set is created, and the ID of the set is returned in the response. If an @attachmentSetId@ is specified, the attachments are added to the specified set, if it exists.
--
-- /Note:/ Consider using 'attachmentSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aatsAttachmentSetId :: Lens.Lens' AddAttachmentsToSet (Core.Maybe Types.AttachmentSetId)
aatsAttachmentSetId = Lens.field @"attachmentSetId"
{-# INLINEABLE aatsAttachmentSetId #-}
{-# DEPRECATED attachmentSetId "Use generic-lens or generic-optics with 'attachmentSetId' instead"  #-}

instance Core.ToQuery AddAttachmentsToSet where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders AddAttachmentsToSet where
        toHeaders AddAttachmentsToSet{..}
          = Core.pure
              ("X-Amz-Target", "AWSSupport_20130415.AddAttachmentsToSet")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON AddAttachmentsToSet where
        toJSON AddAttachmentsToSet{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("attachments" Core..= attachments),
                  ("attachmentSetId" Core..=) Core.<$> attachmentSetId])

instance Core.AWSRequest AddAttachmentsToSet where
        type Rs AddAttachmentsToSet = AddAttachmentsToSetResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 AddAttachmentsToSetResponse' Core.<$>
                   (x Core..:? "attachmentSetId") Core.<*> x Core..:? "expiryTime"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The ID and expiry time of the attachment set returned by the 'AddAttachmentsToSet' operation.
--
-- /See:/ 'mkAddAttachmentsToSetResponse' smart constructor.
data AddAttachmentsToSetResponse = AddAttachmentsToSetResponse'
  { attachmentSetId :: Core.Maybe Types.AttachmentSetId
    -- ^ The ID of the attachment set. If an @attachmentSetId@ was not specified, a new attachment set is created, and the ID of the set is returned in the response. If an @attachmentSetId@ was specified, the attachments are added to the specified set, if it exists.
  , expiryTime :: Core.Maybe Types.ExpiryTime
    -- ^ The time and date when the attachment set expires.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AddAttachmentsToSetResponse' value with any optional fields omitted.
mkAddAttachmentsToSetResponse
    :: Core.Int -- ^ 'responseStatus'
    -> AddAttachmentsToSetResponse
mkAddAttachmentsToSetResponse responseStatus
  = AddAttachmentsToSetResponse'{attachmentSetId = Core.Nothing,
                                 expiryTime = Core.Nothing, responseStatus}

-- | The ID of the attachment set. If an @attachmentSetId@ was not specified, a new attachment set is created, and the ID of the set is returned in the response. If an @attachmentSetId@ was specified, the attachments are added to the specified set, if it exists.
--
-- /Note:/ Consider using 'attachmentSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aatsrrsAttachmentSetId :: Lens.Lens' AddAttachmentsToSetResponse (Core.Maybe Types.AttachmentSetId)
aatsrrsAttachmentSetId = Lens.field @"attachmentSetId"
{-# INLINEABLE aatsrrsAttachmentSetId #-}
{-# DEPRECATED attachmentSetId "Use generic-lens or generic-optics with 'attachmentSetId' instead"  #-}

-- | The time and date when the attachment set expires.
--
-- /Note:/ Consider using 'expiryTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aatsrrsExpiryTime :: Lens.Lens' AddAttachmentsToSetResponse (Core.Maybe Types.ExpiryTime)
aatsrrsExpiryTime = Lens.field @"expiryTime"
{-# INLINEABLE aatsrrsExpiryTime #-}
{-# DEPRECATED expiryTime "Use generic-lens or generic-optics with 'expiryTime' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aatsrrsResponseStatus :: Lens.Lens' AddAttachmentsToSetResponse Core.Int
aatsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE aatsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

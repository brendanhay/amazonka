{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    AddAttachmentsToSet (..),
    mkAddAttachmentsToSet,

    -- ** Request lenses
    aatsAttachments,
    aatsAttachmentSetId,

    -- * Destructuring the response
    AddAttachmentsToSetResponse (..),
    mkAddAttachmentsToSetResponse,

    -- ** Response lenses
    aatsrsExpiryTime,
    aatsrsAttachmentSetId,
    aatsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Support.Types

-- | /See:/ 'mkAddAttachmentsToSet' smart constructor.
data AddAttachmentsToSet = AddAttachmentsToSet'
  { -- | One or more attachments to add to the set. You can add up to three attachments per set. The size limit is 5 MB per attachment.
    --
    -- In the @Attachment@ object, use the @data@ parameter to specify the contents of the attachment file. In the previous request syntax, the value for @data@ appear as @blob@ , which is represented as a base64-encoded string. The value for @fileName@ is the name of the attachment, such as @troubleshoot-screenshot.png@ .
    attachments :: [Attachment],
    -- | The ID of the attachment set. If an @attachmentSetId@ is not specified, a new attachment set is created, and the ID of the set is returned in the response. If an @attachmentSetId@ is specified, the attachments are added to the specified set, if it exists.
    attachmentSetId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AddAttachmentsToSet' with the minimum fields required to make a request.
--
-- * 'attachments' - One or more attachments to add to the set. You can add up to three attachments per set. The size limit is 5 MB per attachment.
--
-- In the @Attachment@ object, use the @data@ parameter to specify the contents of the attachment file. In the previous request syntax, the value for @data@ appear as @blob@ , which is represented as a base64-encoded string. The value for @fileName@ is the name of the attachment, such as @troubleshoot-screenshot.png@ .
-- * 'attachmentSetId' - The ID of the attachment set. If an @attachmentSetId@ is not specified, a new attachment set is created, and the ID of the set is returned in the response. If an @attachmentSetId@ is specified, the attachments are added to the specified set, if it exists.
mkAddAttachmentsToSet ::
  AddAttachmentsToSet
mkAddAttachmentsToSet =
  AddAttachmentsToSet'
    { attachments = Lude.mempty,
      attachmentSetId = Lude.Nothing
    }

-- | One or more attachments to add to the set. You can add up to three attachments per set. The size limit is 5 MB per attachment.
--
-- In the @Attachment@ object, use the @data@ parameter to specify the contents of the attachment file. In the previous request syntax, the value for @data@ appear as @blob@ , which is represented as a base64-encoded string. The value for @fileName@ is the name of the attachment, such as @troubleshoot-screenshot.png@ .
--
-- /Note:/ Consider using 'attachments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aatsAttachments :: Lens.Lens' AddAttachmentsToSet [Attachment]
aatsAttachments = Lens.lens (attachments :: AddAttachmentsToSet -> [Attachment]) (\s a -> s {attachments = a} :: AddAttachmentsToSet)
{-# DEPRECATED aatsAttachments "Use generic-lens or generic-optics with 'attachments' instead." #-}

-- | The ID of the attachment set. If an @attachmentSetId@ is not specified, a new attachment set is created, and the ID of the set is returned in the response. If an @attachmentSetId@ is specified, the attachments are added to the specified set, if it exists.
--
-- /Note:/ Consider using 'attachmentSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aatsAttachmentSetId :: Lens.Lens' AddAttachmentsToSet (Lude.Maybe Lude.Text)
aatsAttachmentSetId = Lens.lens (attachmentSetId :: AddAttachmentsToSet -> Lude.Maybe Lude.Text) (\s a -> s {attachmentSetId = a} :: AddAttachmentsToSet)
{-# DEPRECATED aatsAttachmentSetId "Use generic-lens or generic-optics with 'attachmentSetId' instead." #-}

instance Lude.AWSRequest AddAttachmentsToSet where
  type Rs AddAttachmentsToSet = AddAttachmentsToSetResponse
  request = Req.postJSON supportService
  response =
    Res.receiveJSON
      ( \s h x ->
          AddAttachmentsToSetResponse'
            Lude.<$> (x Lude..?> "expiryTime")
            Lude.<*> (x Lude..?> "attachmentSetId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AddAttachmentsToSet where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSSupport_20130415.AddAttachmentsToSet" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AddAttachmentsToSet where
  toJSON AddAttachmentsToSet' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("attachments" Lude..= attachments),
            ("attachmentSetId" Lude..=) Lude.<$> attachmentSetId
          ]
      )

instance Lude.ToPath AddAttachmentsToSet where
  toPath = Lude.const "/"

instance Lude.ToQuery AddAttachmentsToSet where
  toQuery = Lude.const Lude.mempty

-- | The ID and expiry time of the attachment set returned by the 'AddAttachmentsToSet' operation.
--
-- /See:/ 'mkAddAttachmentsToSetResponse' smart constructor.
data AddAttachmentsToSetResponse = AddAttachmentsToSetResponse'
  { -- | The time and date when the attachment set expires.
    expiryTime :: Lude.Maybe Lude.Text,
    -- | The ID of the attachment set. If an @attachmentSetId@ was not specified, a new attachment set is created, and the ID of the set is returned in the response. If an @attachmentSetId@ was specified, the attachments are added to the specified set, if it exists.
    attachmentSetId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AddAttachmentsToSetResponse' with the minimum fields required to make a request.
--
-- * 'expiryTime' - The time and date when the attachment set expires.
-- * 'attachmentSetId' - The ID of the attachment set. If an @attachmentSetId@ was not specified, a new attachment set is created, and the ID of the set is returned in the response. If an @attachmentSetId@ was specified, the attachments are added to the specified set, if it exists.
-- * 'responseStatus' - The response status code.
mkAddAttachmentsToSetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AddAttachmentsToSetResponse
mkAddAttachmentsToSetResponse pResponseStatus_ =
  AddAttachmentsToSetResponse'
    { expiryTime = Lude.Nothing,
      attachmentSetId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The time and date when the attachment set expires.
--
-- /Note:/ Consider using 'expiryTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aatsrsExpiryTime :: Lens.Lens' AddAttachmentsToSetResponse (Lude.Maybe Lude.Text)
aatsrsExpiryTime = Lens.lens (expiryTime :: AddAttachmentsToSetResponse -> Lude.Maybe Lude.Text) (\s a -> s {expiryTime = a} :: AddAttachmentsToSetResponse)
{-# DEPRECATED aatsrsExpiryTime "Use generic-lens or generic-optics with 'expiryTime' instead." #-}

-- | The ID of the attachment set. If an @attachmentSetId@ was not specified, a new attachment set is created, and the ID of the set is returned in the response. If an @attachmentSetId@ was specified, the attachments are added to the specified set, if it exists.
--
-- /Note:/ Consider using 'attachmentSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aatsrsAttachmentSetId :: Lens.Lens' AddAttachmentsToSetResponse (Lude.Maybe Lude.Text)
aatsrsAttachmentSetId = Lens.lens (attachmentSetId :: AddAttachmentsToSetResponse -> Lude.Maybe Lude.Text) (\s a -> s {attachmentSetId = a} :: AddAttachmentsToSetResponse)
{-# DEPRECATED aatsrsAttachmentSetId "Use generic-lens or generic-optics with 'attachmentSetId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aatsrsResponseStatus :: Lens.Lens' AddAttachmentsToSetResponse Lude.Int
aatsrsResponseStatus = Lens.lens (responseStatus :: AddAttachmentsToSetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AddAttachmentsToSetResponse)
{-# DEPRECATED aatsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

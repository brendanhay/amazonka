{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Support.AddAttachmentsToSet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds one or more attachments to an attachment set.
--
-- An attachment set is a temporary container for attachments that you add
-- to a case or case communication. The set is available for 1 hour after
-- it\'s created. The @expiryTime@ returned in the response is when the set
-- expires.
--
-- -   You must have a Business or Enterprise support plan to use the AWS
--     Support API.
--
-- -   If you call the AWS Support API from an account that does not have a
--     Business or Enterprise support plan, the
--     @SubscriptionRequiredException@ error message appears. For
--     information about changing your support plan, see
--     <http://aws.amazon.com/premiumsupport/ AWS Support>.
module Network.AWS.Support.AddAttachmentsToSet
  ( -- * Creating a Request
    AddAttachmentsToSet (..),
    newAddAttachmentsToSet,

    -- * Request Lenses
    addAttachmentsToSet_attachmentSetId,
    addAttachmentsToSet_attachments,

    -- * Destructuring the Response
    AddAttachmentsToSetResponse (..),
    newAddAttachmentsToSetResponse,

    -- * Response Lenses
    addAttachmentsToSetResponse_expiryTime,
    addAttachmentsToSetResponse_attachmentSetId,
    addAttachmentsToSetResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Support.Types

-- | /See:/ 'newAddAttachmentsToSet' smart constructor.
data AddAttachmentsToSet = AddAttachmentsToSet'
  { -- | The ID of the attachment set. If an @attachmentSetId@ is not specified,
    -- a new attachment set is created, and the ID of the set is returned in
    -- the response. If an @attachmentSetId@ is specified, the attachments are
    -- added to the specified set, if it exists.
    attachmentSetId :: Core.Maybe Core.Text,
    -- | One or more attachments to add to the set. You can add up to three
    -- attachments per set. The size limit is 5 MB per attachment.
    --
    -- In the @Attachment@ object, use the @data@ parameter to specify the
    -- contents of the attachment file. In the previous request syntax, the
    -- value for @data@ appear as @blob@, which is represented as a
    -- base64-encoded string. The value for @fileName@ is the name of the
    -- attachment, such as @troubleshoot-screenshot.png@.
    attachments :: [Attachment]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AddAttachmentsToSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attachmentSetId', 'addAttachmentsToSet_attachmentSetId' - The ID of the attachment set. If an @attachmentSetId@ is not specified,
-- a new attachment set is created, and the ID of the set is returned in
-- the response. If an @attachmentSetId@ is specified, the attachments are
-- added to the specified set, if it exists.
--
-- 'attachments', 'addAttachmentsToSet_attachments' - One or more attachments to add to the set. You can add up to three
-- attachments per set. The size limit is 5 MB per attachment.
--
-- In the @Attachment@ object, use the @data@ parameter to specify the
-- contents of the attachment file. In the previous request syntax, the
-- value for @data@ appear as @blob@, which is represented as a
-- base64-encoded string. The value for @fileName@ is the name of the
-- attachment, such as @troubleshoot-screenshot.png@.
newAddAttachmentsToSet ::
  AddAttachmentsToSet
newAddAttachmentsToSet =
  AddAttachmentsToSet'
    { attachmentSetId =
        Core.Nothing,
      attachments = Core.mempty
    }

-- | The ID of the attachment set. If an @attachmentSetId@ is not specified,
-- a new attachment set is created, and the ID of the set is returned in
-- the response. If an @attachmentSetId@ is specified, the attachments are
-- added to the specified set, if it exists.
addAttachmentsToSet_attachmentSetId :: Lens.Lens' AddAttachmentsToSet (Core.Maybe Core.Text)
addAttachmentsToSet_attachmentSetId = Lens.lens (\AddAttachmentsToSet' {attachmentSetId} -> attachmentSetId) (\s@AddAttachmentsToSet' {} a -> s {attachmentSetId = a} :: AddAttachmentsToSet)

-- | One or more attachments to add to the set. You can add up to three
-- attachments per set. The size limit is 5 MB per attachment.
--
-- In the @Attachment@ object, use the @data@ parameter to specify the
-- contents of the attachment file. In the previous request syntax, the
-- value for @data@ appear as @blob@, which is represented as a
-- base64-encoded string. The value for @fileName@ is the name of the
-- attachment, such as @troubleshoot-screenshot.png@.
addAttachmentsToSet_attachments :: Lens.Lens' AddAttachmentsToSet [Attachment]
addAttachmentsToSet_attachments = Lens.lens (\AddAttachmentsToSet' {attachments} -> attachments) (\s@AddAttachmentsToSet' {} a -> s {attachments = a} :: AddAttachmentsToSet) Core.. Lens._Coerce

instance Core.AWSRequest AddAttachmentsToSet where
  type
    AWSResponse AddAttachmentsToSet =
      AddAttachmentsToSetResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          AddAttachmentsToSetResponse'
            Core.<$> (x Core..?> "expiryTime")
            Core.<*> (x Core..?> "attachmentSetId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable AddAttachmentsToSet

instance Core.NFData AddAttachmentsToSet

instance Core.ToHeaders AddAttachmentsToSet where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSSupport_20130415.AddAttachmentsToSet" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON AddAttachmentsToSet where
  toJSON AddAttachmentsToSet' {..} =
    Core.object
      ( Core.catMaybes
          [ ("attachmentSetId" Core..=)
              Core.<$> attachmentSetId,
            Core.Just ("attachments" Core..= attachments)
          ]
      )

instance Core.ToPath AddAttachmentsToSet where
  toPath = Core.const "/"

instance Core.ToQuery AddAttachmentsToSet where
  toQuery = Core.const Core.mempty

-- | The ID and expiry time of the attachment set returned by the
-- AddAttachmentsToSet operation.
--
-- /See:/ 'newAddAttachmentsToSetResponse' smart constructor.
data AddAttachmentsToSetResponse = AddAttachmentsToSetResponse'
  { -- | The time and date when the attachment set expires.
    expiryTime :: Core.Maybe Core.Text,
    -- | The ID of the attachment set. If an @attachmentSetId@ was not specified,
    -- a new attachment set is created, and the ID of the set is returned in
    -- the response. If an @attachmentSetId@ was specified, the attachments are
    -- added to the specified set, if it exists.
    attachmentSetId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AddAttachmentsToSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expiryTime', 'addAttachmentsToSetResponse_expiryTime' - The time and date when the attachment set expires.
--
-- 'attachmentSetId', 'addAttachmentsToSetResponse_attachmentSetId' - The ID of the attachment set. If an @attachmentSetId@ was not specified,
-- a new attachment set is created, and the ID of the set is returned in
-- the response. If an @attachmentSetId@ was specified, the attachments are
-- added to the specified set, if it exists.
--
-- 'httpStatus', 'addAttachmentsToSetResponse_httpStatus' - The response's http status code.
newAddAttachmentsToSetResponse ::
  -- | 'httpStatus'
  Core.Int ->
  AddAttachmentsToSetResponse
newAddAttachmentsToSetResponse pHttpStatus_ =
  AddAttachmentsToSetResponse'
    { expiryTime =
        Core.Nothing,
      attachmentSetId = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The time and date when the attachment set expires.
addAttachmentsToSetResponse_expiryTime :: Lens.Lens' AddAttachmentsToSetResponse (Core.Maybe Core.Text)
addAttachmentsToSetResponse_expiryTime = Lens.lens (\AddAttachmentsToSetResponse' {expiryTime} -> expiryTime) (\s@AddAttachmentsToSetResponse' {} a -> s {expiryTime = a} :: AddAttachmentsToSetResponse)

-- | The ID of the attachment set. If an @attachmentSetId@ was not specified,
-- a new attachment set is created, and the ID of the set is returned in
-- the response. If an @attachmentSetId@ was specified, the attachments are
-- added to the specified set, if it exists.
addAttachmentsToSetResponse_attachmentSetId :: Lens.Lens' AddAttachmentsToSetResponse (Core.Maybe Core.Text)
addAttachmentsToSetResponse_attachmentSetId = Lens.lens (\AddAttachmentsToSetResponse' {attachmentSetId} -> attachmentSetId) (\s@AddAttachmentsToSetResponse' {} a -> s {attachmentSetId = a} :: AddAttachmentsToSetResponse)

-- | The response's http status code.
addAttachmentsToSetResponse_httpStatus :: Lens.Lens' AddAttachmentsToSetResponse Core.Int
addAttachmentsToSetResponse_httpStatus = Lens.lens (\AddAttachmentsToSetResponse' {httpStatus} -> httpStatus) (\s@AddAttachmentsToSetResponse' {} a -> s {httpStatus = a} :: AddAttachmentsToSetResponse)

instance Core.NFData AddAttachmentsToSetResponse

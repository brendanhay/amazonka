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
-- Module      : Amazonka.Support.AddAttachmentsToSet
-- Copyright   : (c) 2013-2023 Brendan Hay
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
-- -   You must have a Business, Enterprise On-Ramp, or Enterprise Support
--     plan to use the Amazon Web Services Support API.
--
-- -   If you call the Amazon Web Services Support API from an account that
--     doesn\'t have a Business, Enterprise On-Ramp, or Enterprise Support
--     plan, the @SubscriptionRequiredException@ error message appears. For
--     information about changing your support plan, see
--     <http://aws.amazon.com/premiumsupport/ Amazon Web Services Support>.
module Amazonka.Support.AddAttachmentsToSet
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
    addAttachmentsToSetResponse_attachmentSetId,
    addAttachmentsToSetResponse_expiryTime,
    addAttachmentsToSetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Support.Types

-- | /See:/ 'newAddAttachmentsToSet' smart constructor.
data AddAttachmentsToSet = AddAttachmentsToSet'
  { -- | The ID of the attachment set. If an @attachmentSetId@ is not specified,
    -- a new attachment set is created, and the ID of the set is returned in
    -- the response. If an @attachmentSetId@ is specified, the attachments are
    -- added to the specified set, if it exists.
    attachmentSetId :: Prelude.Maybe Prelude.Text,
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
        Prelude.Nothing,
      attachments = Prelude.mempty
    }

-- | The ID of the attachment set. If an @attachmentSetId@ is not specified,
-- a new attachment set is created, and the ID of the set is returned in
-- the response. If an @attachmentSetId@ is specified, the attachments are
-- added to the specified set, if it exists.
addAttachmentsToSet_attachmentSetId :: Lens.Lens' AddAttachmentsToSet (Prelude.Maybe Prelude.Text)
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
addAttachmentsToSet_attachments = Lens.lens (\AddAttachmentsToSet' {attachments} -> attachments) (\s@AddAttachmentsToSet' {} a -> s {attachments = a} :: AddAttachmentsToSet) Prelude.. Lens.coerced

instance Core.AWSRequest AddAttachmentsToSet where
  type
    AWSResponse AddAttachmentsToSet =
      AddAttachmentsToSetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AddAttachmentsToSetResponse'
            Prelude.<$> (x Data..?> "attachmentSetId")
            Prelude.<*> (x Data..?> "expiryTime")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AddAttachmentsToSet where
  hashWithSalt _salt AddAttachmentsToSet' {..} =
    _salt
      `Prelude.hashWithSalt` attachmentSetId
      `Prelude.hashWithSalt` attachments

instance Prelude.NFData AddAttachmentsToSet where
  rnf AddAttachmentsToSet' {..} =
    Prelude.rnf attachmentSetId
      `Prelude.seq` Prelude.rnf attachments

instance Data.ToHeaders AddAttachmentsToSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSSupport_20130415.AddAttachmentsToSet" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AddAttachmentsToSet where
  toJSON AddAttachmentsToSet' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("attachmentSetId" Data..=)
              Prelude.<$> attachmentSetId,
            Prelude.Just ("attachments" Data..= attachments)
          ]
      )

instance Data.ToPath AddAttachmentsToSet where
  toPath = Prelude.const "/"

instance Data.ToQuery AddAttachmentsToSet where
  toQuery = Prelude.const Prelude.mempty

-- | The ID and expiry time of the attachment set returned by the
-- AddAttachmentsToSet operation.
--
-- /See:/ 'newAddAttachmentsToSetResponse' smart constructor.
data AddAttachmentsToSetResponse = AddAttachmentsToSetResponse'
  { -- | The ID of the attachment set. If an @attachmentSetId@ was not specified,
    -- a new attachment set is created, and the ID of the set is returned in
    -- the response. If an @attachmentSetId@ was specified, the attachments are
    -- added to the specified set, if it exists.
    attachmentSetId :: Prelude.Maybe Prelude.Text,
    -- | The time and date when the attachment set expires.
    expiryTime :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddAttachmentsToSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attachmentSetId', 'addAttachmentsToSetResponse_attachmentSetId' - The ID of the attachment set. If an @attachmentSetId@ was not specified,
-- a new attachment set is created, and the ID of the set is returned in
-- the response. If an @attachmentSetId@ was specified, the attachments are
-- added to the specified set, if it exists.
--
-- 'expiryTime', 'addAttachmentsToSetResponse_expiryTime' - The time and date when the attachment set expires.
--
-- 'httpStatus', 'addAttachmentsToSetResponse_httpStatus' - The response's http status code.
newAddAttachmentsToSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AddAttachmentsToSetResponse
newAddAttachmentsToSetResponse pHttpStatus_ =
  AddAttachmentsToSetResponse'
    { attachmentSetId =
        Prelude.Nothing,
      expiryTime = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the attachment set. If an @attachmentSetId@ was not specified,
-- a new attachment set is created, and the ID of the set is returned in
-- the response. If an @attachmentSetId@ was specified, the attachments are
-- added to the specified set, if it exists.
addAttachmentsToSetResponse_attachmentSetId :: Lens.Lens' AddAttachmentsToSetResponse (Prelude.Maybe Prelude.Text)
addAttachmentsToSetResponse_attachmentSetId = Lens.lens (\AddAttachmentsToSetResponse' {attachmentSetId} -> attachmentSetId) (\s@AddAttachmentsToSetResponse' {} a -> s {attachmentSetId = a} :: AddAttachmentsToSetResponse)

-- | The time and date when the attachment set expires.
addAttachmentsToSetResponse_expiryTime :: Lens.Lens' AddAttachmentsToSetResponse (Prelude.Maybe Prelude.Text)
addAttachmentsToSetResponse_expiryTime = Lens.lens (\AddAttachmentsToSetResponse' {expiryTime} -> expiryTime) (\s@AddAttachmentsToSetResponse' {} a -> s {expiryTime = a} :: AddAttachmentsToSetResponse)

-- | The response's http status code.
addAttachmentsToSetResponse_httpStatus :: Lens.Lens' AddAttachmentsToSetResponse Prelude.Int
addAttachmentsToSetResponse_httpStatus = Lens.lens (\AddAttachmentsToSetResponse' {httpStatus} -> httpStatus) (\s@AddAttachmentsToSetResponse' {} a -> s {httpStatus = a} :: AddAttachmentsToSetResponse)

instance Prelude.NFData AddAttachmentsToSetResponse where
  rnf AddAttachmentsToSetResponse' {..} =
    Prelude.rnf attachmentSetId
      `Prelude.seq` Prelude.rnf expiryTime
      `Prelude.seq` Prelude.rnf httpStatus

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
-- Module      : Network.AWS.Support.AddCommunicationToCase
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds additional customer communication to an AWS Support case. Use the
-- @caseId@ parameter to identify the case to which to add communication.
-- You can list a set of email addresses to copy on the communication by
-- using the @ccEmailAddresses@ parameter. The @communicationBody@ value
-- contains the text of the communication.
--
-- -   You must have a Business or Enterprise support plan to use the AWS
--     Support API.
--
-- -   If you call the AWS Support API from an account that does not have a
--     Business or Enterprise support plan, the
--     @SubscriptionRequiredException@ error message appears. For
--     information about changing your support plan, see
--     <http://aws.amazon.com/premiumsupport/ AWS Support>.
module Network.AWS.Support.AddCommunicationToCase
  ( -- * Creating a Request
    AddCommunicationToCase (..),
    newAddCommunicationToCase,

    -- * Request Lenses
    addCommunicationToCase_caseId,
    addCommunicationToCase_ccEmailAddresses,
    addCommunicationToCase_attachmentSetId,
    addCommunicationToCase_communicationBody,

    -- * Destructuring the Response
    AddCommunicationToCaseResponse (..),
    newAddCommunicationToCaseResponse,

    -- * Response Lenses
    addCommunicationToCaseResponse_result,
    addCommunicationToCaseResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Support.Types

-- | /See:/ 'newAddCommunicationToCase' smart constructor.
data AddCommunicationToCase = AddCommunicationToCase'
  { -- | The AWS Support case ID requested or returned in the call. The case ID
    -- is an alphanumeric string formatted as shown in this example:
    -- case-/12345678910-2013-c4c1d2bf33c5cf47/
    caseId :: Core.Maybe Core.Text,
    -- | The email addresses in the CC line of an email to be added to the
    -- support case.
    ccEmailAddresses :: Core.Maybe [Core.Text],
    -- | The ID of a set of one or more attachments for the communication to add
    -- to the case. Create the set by calling AddAttachmentsToSet
    attachmentSetId :: Core.Maybe Core.Text,
    -- | The body of an email communication to add to the support case.
    communicationBody :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AddCommunicationToCase' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'caseId', 'addCommunicationToCase_caseId' - The AWS Support case ID requested or returned in the call. The case ID
-- is an alphanumeric string formatted as shown in this example:
-- case-/12345678910-2013-c4c1d2bf33c5cf47/
--
-- 'ccEmailAddresses', 'addCommunicationToCase_ccEmailAddresses' - The email addresses in the CC line of an email to be added to the
-- support case.
--
-- 'attachmentSetId', 'addCommunicationToCase_attachmentSetId' - The ID of a set of one or more attachments for the communication to add
-- to the case. Create the set by calling AddAttachmentsToSet
--
-- 'communicationBody', 'addCommunicationToCase_communicationBody' - The body of an email communication to add to the support case.
newAddCommunicationToCase ::
  -- | 'communicationBody'
  Core.Text ->
  AddCommunicationToCase
newAddCommunicationToCase pCommunicationBody_ =
  AddCommunicationToCase'
    { caseId = Core.Nothing,
      ccEmailAddresses = Core.Nothing,
      attachmentSetId = Core.Nothing,
      communicationBody = pCommunicationBody_
    }

-- | The AWS Support case ID requested or returned in the call. The case ID
-- is an alphanumeric string formatted as shown in this example:
-- case-/12345678910-2013-c4c1d2bf33c5cf47/
addCommunicationToCase_caseId :: Lens.Lens' AddCommunicationToCase (Core.Maybe Core.Text)
addCommunicationToCase_caseId = Lens.lens (\AddCommunicationToCase' {caseId} -> caseId) (\s@AddCommunicationToCase' {} a -> s {caseId = a} :: AddCommunicationToCase)

-- | The email addresses in the CC line of an email to be added to the
-- support case.
addCommunicationToCase_ccEmailAddresses :: Lens.Lens' AddCommunicationToCase (Core.Maybe [Core.Text])
addCommunicationToCase_ccEmailAddresses = Lens.lens (\AddCommunicationToCase' {ccEmailAddresses} -> ccEmailAddresses) (\s@AddCommunicationToCase' {} a -> s {ccEmailAddresses = a} :: AddCommunicationToCase) Core.. Lens.mapping Lens._Coerce

-- | The ID of a set of one or more attachments for the communication to add
-- to the case. Create the set by calling AddAttachmentsToSet
addCommunicationToCase_attachmentSetId :: Lens.Lens' AddCommunicationToCase (Core.Maybe Core.Text)
addCommunicationToCase_attachmentSetId = Lens.lens (\AddCommunicationToCase' {attachmentSetId} -> attachmentSetId) (\s@AddCommunicationToCase' {} a -> s {attachmentSetId = a} :: AddCommunicationToCase)

-- | The body of an email communication to add to the support case.
addCommunicationToCase_communicationBody :: Lens.Lens' AddCommunicationToCase Core.Text
addCommunicationToCase_communicationBody = Lens.lens (\AddCommunicationToCase' {communicationBody} -> communicationBody) (\s@AddCommunicationToCase' {} a -> s {communicationBody = a} :: AddCommunicationToCase)

instance Core.AWSRequest AddCommunicationToCase where
  type
    AWSResponse AddCommunicationToCase =
      AddCommunicationToCaseResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          AddCommunicationToCaseResponse'
            Core.<$> (x Core..?> "result")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable AddCommunicationToCase

instance Core.NFData AddCommunicationToCase

instance Core.ToHeaders AddCommunicationToCase where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSSupport_20130415.AddCommunicationToCase" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON AddCommunicationToCase where
  toJSON AddCommunicationToCase' {..} =
    Core.object
      ( Core.catMaybes
          [ ("caseId" Core..=) Core.<$> caseId,
            ("ccEmailAddresses" Core..=)
              Core.<$> ccEmailAddresses,
            ("attachmentSetId" Core..=) Core.<$> attachmentSetId,
            Core.Just
              ("communicationBody" Core..= communicationBody)
          ]
      )

instance Core.ToPath AddCommunicationToCase where
  toPath = Core.const "/"

instance Core.ToQuery AddCommunicationToCase where
  toQuery = Core.const Core.mempty

-- | The result of the AddCommunicationToCase operation.
--
-- /See:/ 'newAddCommunicationToCaseResponse' smart constructor.
data AddCommunicationToCaseResponse = AddCommunicationToCaseResponse'
  { -- | True if AddCommunicationToCase succeeds. Otherwise, returns an error.
    result :: Core.Maybe Core.Bool,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AddCommunicationToCaseResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'result', 'addCommunicationToCaseResponse_result' - True if AddCommunicationToCase succeeds. Otherwise, returns an error.
--
-- 'httpStatus', 'addCommunicationToCaseResponse_httpStatus' - The response's http status code.
newAddCommunicationToCaseResponse ::
  -- | 'httpStatus'
  Core.Int ->
  AddCommunicationToCaseResponse
newAddCommunicationToCaseResponse pHttpStatus_ =
  AddCommunicationToCaseResponse'
    { result =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | True if AddCommunicationToCase succeeds. Otherwise, returns an error.
addCommunicationToCaseResponse_result :: Lens.Lens' AddCommunicationToCaseResponse (Core.Maybe Core.Bool)
addCommunicationToCaseResponse_result = Lens.lens (\AddCommunicationToCaseResponse' {result} -> result) (\s@AddCommunicationToCaseResponse' {} a -> s {result = a} :: AddCommunicationToCaseResponse)

-- | The response's http status code.
addCommunicationToCaseResponse_httpStatus :: Lens.Lens' AddCommunicationToCaseResponse Core.Int
addCommunicationToCaseResponse_httpStatus = Lens.lens (\AddCommunicationToCaseResponse' {httpStatus} -> httpStatus) (\s@AddCommunicationToCaseResponse' {} a -> s {httpStatus = a} :: AddCommunicationToCaseResponse)

instance Core.NFData AddCommunicationToCaseResponse

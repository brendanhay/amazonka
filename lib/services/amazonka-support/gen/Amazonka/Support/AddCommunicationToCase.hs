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
-- Module      : Amazonka.Support.AddCommunicationToCase
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds additional customer communication to an Amazon Web Services Support
-- case. Use the @caseId@ parameter to identify the case to which to add
-- communication. You can list a set of email addresses to copy on the
-- communication by using the @ccEmailAddresses@ parameter. The
-- @communicationBody@ value contains the text of the communication.
--
-- -   You must have a Business, Enterprise On-Ramp, or Enterprise Support
--     plan to use the Amazon Web Services Support API.
--
-- -   If you call the Amazon Web Services Support API from an account that
--     does not have a Business, Enterprise On-Ramp, or Enterprise Support
--     plan, the @SubscriptionRequiredException@ error message appears. For
--     information about changing your support plan, see
--     <http://aws.amazon.com/premiumsupport/ Amazon Web Services Support>.
module Amazonka.Support.AddCommunicationToCase
  ( -- * Creating a Request
    AddCommunicationToCase (..),
    newAddCommunicationToCase,

    -- * Request Lenses
    addCommunicationToCase_ccEmailAddresses,
    addCommunicationToCase_caseId,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Support.Types

-- | /See:/ 'newAddCommunicationToCase' smart constructor.
data AddCommunicationToCase = AddCommunicationToCase'
  { -- | The email addresses in the CC line of an email to be added to the
    -- support case.
    ccEmailAddresses :: Prelude.Maybe [Prelude.Text],
    -- | The support case ID requested or returned in the call. The case ID is an
    -- alphanumeric string formatted as shown in this example:
    -- case-/12345678910-2013-c4c1d2bf33c5cf47/
    caseId :: Prelude.Maybe Prelude.Text,
    -- | The ID of a set of one or more attachments for the communication to add
    -- to the case. Create the set by calling AddAttachmentsToSet
    attachmentSetId :: Prelude.Maybe Prelude.Text,
    -- | The body of an email communication to add to the support case.
    communicationBody :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddCommunicationToCase' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ccEmailAddresses', 'addCommunicationToCase_ccEmailAddresses' - The email addresses in the CC line of an email to be added to the
-- support case.
--
-- 'caseId', 'addCommunicationToCase_caseId' - The support case ID requested or returned in the call. The case ID is an
-- alphanumeric string formatted as shown in this example:
-- case-/12345678910-2013-c4c1d2bf33c5cf47/
--
-- 'attachmentSetId', 'addCommunicationToCase_attachmentSetId' - The ID of a set of one or more attachments for the communication to add
-- to the case. Create the set by calling AddAttachmentsToSet
--
-- 'communicationBody', 'addCommunicationToCase_communicationBody' - The body of an email communication to add to the support case.
newAddCommunicationToCase ::
  -- | 'communicationBody'
  Prelude.Text ->
  AddCommunicationToCase
newAddCommunicationToCase pCommunicationBody_ =
  AddCommunicationToCase'
    { ccEmailAddresses =
        Prelude.Nothing,
      caseId = Prelude.Nothing,
      attachmentSetId = Prelude.Nothing,
      communicationBody = pCommunicationBody_
    }

-- | The email addresses in the CC line of an email to be added to the
-- support case.
addCommunicationToCase_ccEmailAddresses :: Lens.Lens' AddCommunicationToCase (Prelude.Maybe [Prelude.Text])
addCommunicationToCase_ccEmailAddresses = Lens.lens (\AddCommunicationToCase' {ccEmailAddresses} -> ccEmailAddresses) (\s@AddCommunicationToCase' {} a -> s {ccEmailAddresses = a} :: AddCommunicationToCase) Prelude.. Lens.mapping Lens.coerced

-- | The support case ID requested or returned in the call. The case ID is an
-- alphanumeric string formatted as shown in this example:
-- case-/12345678910-2013-c4c1d2bf33c5cf47/
addCommunicationToCase_caseId :: Lens.Lens' AddCommunicationToCase (Prelude.Maybe Prelude.Text)
addCommunicationToCase_caseId = Lens.lens (\AddCommunicationToCase' {caseId} -> caseId) (\s@AddCommunicationToCase' {} a -> s {caseId = a} :: AddCommunicationToCase)

-- | The ID of a set of one or more attachments for the communication to add
-- to the case. Create the set by calling AddAttachmentsToSet
addCommunicationToCase_attachmentSetId :: Lens.Lens' AddCommunicationToCase (Prelude.Maybe Prelude.Text)
addCommunicationToCase_attachmentSetId = Lens.lens (\AddCommunicationToCase' {attachmentSetId} -> attachmentSetId) (\s@AddCommunicationToCase' {} a -> s {attachmentSetId = a} :: AddCommunicationToCase)

-- | The body of an email communication to add to the support case.
addCommunicationToCase_communicationBody :: Lens.Lens' AddCommunicationToCase Prelude.Text
addCommunicationToCase_communicationBody = Lens.lens (\AddCommunicationToCase' {communicationBody} -> communicationBody) (\s@AddCommunicationToCase' {} a -> s {communicationBody = a} :: AddCommunicationToCase)

instance Core.AWSRequest AddCommunicationToCase where
  type
    AWSResponse AddCommunicationToCase =
      AddCommunicationToCaseResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AddCommunicationToCaseResponse'
            Prelude.<$> (x Core..?> "result")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AddCommunicationToCase where
  hashWithSalt _salt AddCommunicationToCase' {..} =
    _salt `Prelude.hashWithSalt` ccEmailAddresses
      `Prelude.hashWithSalt` caseId
      `Prelude.hashWithSalt` attachmentSetId
      `Prelude.hashWithSalt` communicationBody

instance Prelude.NFData AddCommunicationToCase where
  rnf AddCommunicationToCase' {..} =
    Prelude.rnf ccEmailAddresses
      `Prelude.seq` Prelude.rnf caseId
      `Prelude.seq` Prelude.rnf attachmentSetId
      `Prelude.seq` Prelude.rnf communicationBody

instance Core.ToHeaders AddCommunicationToCase where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSSupport_20130415.AddCommunicationToCase" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON AddCommunicationToCase where
  toJSON AddCommunicationToCase' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ccEmailAddresses" Core..=)
              Prelude.<$> ccEmailAddresses,
            ("caseId" Core..=) Prelude.<$> caseId,
            ("attachmentSetId" Core..=)
              Prelude.<$> attachmentSetId,
            Prelude.Just
              ("communicationBody" Core..= communicationBody)
          ]
      )

instance Core.ToPath AddCommunicationToCase where
  toPath = Prelude.const "/"

instance Core.ToQuery AddCommunicationToCase where
  toQuery = Prelude.const Prelude.mempty

-- | The result of the AddCommunicationToCase operation.
--
-- /See:/ 'newAddCommunicationToCaseResponse' smart constructor.
data AddCommunicationToCaseResponse = AddCommunicationToCaseResponse'
  { -- | True if AddCommunicationToCase succeeds. Otherwise, returns an error.
    result :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  AddCommunicationToCaseResponse
newAddCommunicationToCaseResponse pHttpStatus_ =
  AddCommunicationToCaseResponse'
    { result =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | True if AddCommunicationToCase succeeds. Otherwise, returns an error.
addCommunicationToCaseResponse_result :: Lens.Lens' AddCommunicationToCaseResponse (Prelude.Maybe Prelude.Bool)
addCommunicationToCaseResponse_result = Lens.lens (\AddCommunicationToCaseResponse' {result} -> result) (\s@AddCommunicationToCaseResponse' {} a -> s {result = a} :: AddCommunicationToCaseResponse)

-- | The response's http status code.
addCommunicationToCaseResponse_httpStatus :: Lens.Lens' AddCommunicationToCaseResponse Prelude.Int
addCommunicationToCaseResponse_httpStatus = Lens.lens (\AddCommunicationToCaseResponse' {httpStatus} -> httpStatus) (\s@AddCommunicationToCaseResponse' {} a -> s {httpStatus = a} :: AddCommunicationToCaseResponse)

instance
  Prelude.NFData
    AddCommunicationToCaseResponse
  where
  rnf AddCommunicationToCaseResponse' {..} =
    Prelude.rnf result
      `Prelude.seq` Prelude.rnf httpStatus

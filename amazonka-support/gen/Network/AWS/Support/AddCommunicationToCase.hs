{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Support.Types

-- | /See:/ 'newAddCommunicationToCase' smart constructor.
data AddCommunicationToCase = AddCommunicationToCase'
  { -- | The AWS Support case ID requested or returned in the call. The case ID
    -- is an alphanumeric string formatted as shown in this example:
    -- case-/12345678910-2013-c4c1d2bf33c5cf47/
    caseId :: Prelude.Maybe Prelude.Text,
    -- | The email addresses in the CC line of an email to be added to the
    -- support case.
    ccEmailAddresses :: Prelude.Maybe [Prelude.Text],
    -- | The ID of a set of one or more attachments for the communication to add
    -- to the case. Create the set by calling AddAttachmentsToSet
    attachmentSetId :: Prelude.Maybe Prelude.Text,
    -- | The body of an email communication to add to the support case.
    communicationBody :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  AddCommunicationToCase
newAddCommunicationToCase pCommunicationBody_ =
  AddCommunicationToCase'
    { caseId = Prelude.Nothing,
      ccEmailAddresses = Prelude.Nothing,
      attachmentSetId = Prelude.Nothing,
      communicationBody = pCommunicationBody_
    }

-- | The AWS Support case ID requested or returned in the call. The case ID
-- is an alphanumeric string formatted as shown in this example:
-- case-/12345678910-2013-c4c1d2bf33c5cf47/
addCommunicationToCase_caseId :: Lens.Lens' AddCommunicationToCase (Prelude.Maybe Prelude.Text)
addCommunicationToCase_caseId = Lens.lens (\AddCommunicationToCase' {caseId} -> caseId) (\s@AddCommunicationToCase' {} a -> s {caseId = a} :: AddCommunicationToCase)

-- | The email addresses in the CC line of an email to be added to the
-- support case.
addCommunicationToCase_ccEmailAddresses :: Lens.Lens' AddCommunicationToCase (Prelude.Maybe [Prelude.Text])
addCommunicationToCase_ccEmailAddresses = Lens.lens (\AddCommunicationToCase' {ccEmailAddresses} -> ccEmailAddresses) (\s@AddCommunicationToCase' {} a -> s {ccEmailAddresses = a} :: AddCommunicationToCase) Prelude.. Lens.mapping Prelude._Coerce

-- | The ID of a set of one or more attachments for the communication to add
-- to the case. Create the set by calling AddAttachmentsToSet
addCommunicationToCase_attachmentSetId :: Lens.Lens' AddCommunicationToCase (Prelude.Maybe Prelude.Text)
addCommunicationToCase_attachmentSetId = Lens.lens (\AddCommunicationToCase' {attachmentSetId} -> attachmentSetId) (\s@AddCommunicationToCase' {} a -> s {attachmentSetId = a} :: AddCommunicationToCase)

-- | The body of an email communication to add to the support case.
addCommunicationToCase_communicationBody :: Lens.Lens' AddCommunicationToCase Prelude.Text
addCommunicationToCase_communicationBody = Lens.lens (\AddCommunicationToCase' {communicationBody} -> communicationBody) (\s@AddCommunicationToCase' {} a -> s {communicationBody = a} :: AddCommunicationToCase)

instance Prelude.AWSRequest AddCommunicationToCase where
  type
    Rs AddCommunicationToCase =
      AddCommunicationToCaseResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          AddCommunicationToCaseResponse'
            Prelude.<$> (x Prelude..?> "result")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AddCommunicationToCase

instance Prelude.NFData AddCommunicationToCase

instance Prelude.ToHeaders AddCommunicationToCase where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSSupport_20130415.AddCommunicationToCase" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON AddCommunicationToCase where
  toJSON AddCommunicationToCase' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("caseId" Prelude..=) Prelude.<$> caseId,
            ("ccEmailAddresses" Prelude..=)
              Prelude.<$> ccEmailAddresses,
            ("attachmentSetId" Prelude..=)
              Prelude.<$> attachmentSetId,
            Prelude.Just
              ("communicationBody" Prelude..= communicationBody)
          ]
      )

instance Prelude.ToPath AddCommunicationToCase where
  toPath = Prelude.const "/"

instance Prelude.ToQuery AddCommunicationToCase where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

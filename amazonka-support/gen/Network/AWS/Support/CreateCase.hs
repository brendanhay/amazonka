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
-- Module      : Network.AWS.Support.CreateCase
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a case in the AWS Support Center. This operation is similar to
-- how you create a case in the AWS Support Center
-- <https://console.aws.amazon.com/support/home#/case/create Create Case>
-- page.
--
-- The AWS Support API doesn\'t support requesting service limit increases.
-- You can submit a service limit increase in the following ways:
--
-- -   Submit a request from the AWS Support Center
--     <https://console.aws.amazon.com/support/home#/case/create Create Case>
--     page.
--
-- -   Use the Service Quotas
--     <https://docs.aws.amazon.com/servicequotas/2019-06-24/apireference/API_RequestServiceQuotaIncrease.html RequestServiceQuotaIncrease>
--     operation.
--
-- A successful @CreateCase@ request returns an AWS Support case number.
-- You can use the DescribeCases operation and specify the case number to
-- get existing AWS Support cases. After you create a case, use the
-- AddCommunicationToCase operation to add additional communication or
-- attachments to an existing case.
--
-- The @caseId@ is separate from the @displayId@ that appears in the
-- <https://console.aws.amazon.com/support AWS Support Center>. Use the
-- DescribeCases operation to get the @displayId@.
--
-- -   You must have a Business or Enterprise support plan to use the AWS
--     Support API.
--
-- -   If you call the AWS Support API from an account that does not have a
--     Business or Enterprise support plan, the
--     @SubscriptionRequiredException@ error message appears. For
--     information about changing your support plan, see
--     <http://aws.amazon.com/premiumsupport/ AWS Support>.
module Network.AWS.Support.CreateCase
  ( -- * Creating a Request
    CreateCase (..),
    newCreateCase,

    -- * Request Lenses
    createCase_serviceCode,
    createCase_categoryCode,
    createCase_ccEmailAddresses,
    createCase_issueType,
    createCase_attachmentSetId,
    createCase_severityCode,
    createCase_language,
    createCase_subject,
    createCase_communicationBody,

    -- * Destructuring the Response
    CreateCaseResponse (..),
    newCreateCaseResponse,

    -- * Response Lenses
    createCaseResponse_caseId,
    createCaseResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Support.Types

-- | /See:/ 'newCreateCase' smart constructor.
data CreateCase = CreateCase'
  { -- | The code for the AWS service. You can use the DescribeServices operation
    -- to get the possible @serviceCode@ values.
    serviceCode :: Core.Maybe Core.Text,
    -- | The category of problem for the AWS Support case. You also use the
    -- DescribeServices operation to get the category code for a service. Each
    -- AWS service defines its own set of category codes.
    categoryCode :: Core.Maybe Core.Text,
    -- | A list of email addresses that AWS Support copies on case
    -- correspondence. AWS Support identifies the account that creates the case
    -- when you specify your AWS credentials in an HTTP POST method or use the
    -- <http://aws.amazon.com/tools/ AWS SDKs>.
    ccEmailAddresses :: Core.Maybe [Core.Text],
    -- | The type of issue for the case. You can specify @customer-service@ or
    -- @technical@. If you don\'t specify a value, the default is @technical@.
    issueType :: Core.Maybe Core.Text,
    -- | The ID of a set of one or more attachments for the case. Create the set
    -- by using the AddAttachmentsToSet operation.
    attachmentSetId :: Core.Maybe Core.Text,
    -- | A value that indicates the urgency of the case. This value determines
    -- the response time according to your service level agreement with AWS
    -- Support. You can use the DescribeSeverityLevels operation to get the
    -- possible values for @severityCode@.
    --
    -- For more information, see SeverityLevel and
    -- <https://docs.aws.amazon.com/awssupport/latest/user/getting-started.html#choosing-severity Choosing a Severity>
    -- in the /AWS Support User Guide/.
    --
    -- The availability of severity levels depends on the support plan for the
    -- AWS account.
    severityCode :: Core.Maybe Core.Text,
    -- | The language in which AWS Support handles the case. You must specify the
    -- ISO 639-1 code for the @language@ parameter if you want support in that
    -- language. Currently, English (\"en\") and Japanese (\"ja\") are
    -- supported.
    language :: Core.Maybe Core.Text,
    -- | The title of the AWS Support case. The title appears in the __Subject__
    -- field on the AWS Support Center
    -- <https://console.aws.amazon.com/support/home#/case/create Create Case>
    -- page.
    subject :: Core.Text,
    -- | The communication body text that describes the issue. This text appears
    -- in the __Description__ field on the AWS Support Center
    -- <https://console.aws.amazon.com/support/home#/case/create Create Case>
    -- page.
    communicationBody :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateCase' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceCode', 'createCase_serviceCode' - The code for the AWS service. You can use the DescribeServices operation
-- to get the possible @serviceCode@ values.
--
-- 'categoryCode', 'createCase_categoryCode' - The category of problem for the AWS Support case. You also use the
-- DescribeServices operation to get the category code for a service. Each
-- AWS service defines its own set of category codes.
--
-- 'ccEmailAddresses', 'createCase_ccEmailAddresses' - A list of email addresses that AWS Support copies on case
-- correspondence. AWS Support identifies the account that creates the case
-- when you specify your AWS credentials in an HTTP POST method or use the
-- <http://aws.amazon.com/tools/ AWS SDKs>.
--
-- 'issueType', 'createCase_issueType' - The type of issue for the case. You can specify @customer-service@ or
-- @technical@. If you don\'t specify a value, the default is @technical@.
--
-- 'attachmentSetId', 'createCase_attachmentSetId' - The ID of a set of one or more attachments for the case. Create the set
-- by using the AddAttachmentsToSet operation.
--
-- 'severityCode', 'createCase_severityCode' - A value that indicates the urgency of the case. This value determines
-- the response time according to your service level agreement with AWS
-- Support. You can use the DescribeSeverityLevels operation to get the
-- possible values for @severityCode@.
--
-- For more information, see SeverityLevel and
-- <https://docs.aws.amazon.com/awssupport/latest/user/getting-started.html#choosing-severity Choosing a Severity>
-- in the /AWS Support User Guide/.
--
-- The availability of severity levels depends on the support plan for the
-- AWS account.
--
-- 'language', 'createCase_language' - The language in which AWS Support handles the case. You must specify the
-- ISO 639-1 code for the @language@ parameter if you want support in that
-- language. Currently, English (\"en\") and Japanese (\"ja\") are
-- supported.
--
-- 'subject', 'createCase_subject' - The title of the AWS Support case. The title appears in the __Subject__
-- field on the AWS Support Center
-- <https://console.aws.amazon.com/support/home#/case/create Create Case>
-- page.
--
-- 'communicationBody', 'createCase_communicationBody' - The communication body text that describes the issue. This text appears
-- in the __Description__ field on the AWS Support Center
-- <https://console.aws.amazon.com/support/home#/case/create Create Case>
-- page.
newCreateCase ::
  -- | 'subject'
  Core.Text ->
  -- | 'communicationBody'
  Core.Text ->
  CreateCase
newCreateCase pSubject_ pCommunicationBody_ =
  CreateCase'
    { serviceCode = Core.Nothing,
      categoryCode = Core.Nothing,
      ccEmailAddresses = Core.Nothing,
      issueType = Core.Nothing,
      attachmentSetId = Core.Nothing,
      severityCode = Core.Nothing,
      language = Core.Nothing,
      subject = pSubject_,
      communicationBody = pCommunicationBody_
    }

-- | The code for the AWS service. You can use the DescribeServices operation
-- to get the possible @serviceCode@ values.
createCase_serviceCode :: Lens.Lens' CreateCase (Core.Maybe Core.Text)
createCase_serviceCode = Lens.lens (\CreateCase' {serviceCode} -> serviceCode) (\s@CreateCase' {} a -> s {serviceCode = a} :: CreateCase)

-- | The category of problem for the AWS Support case. You also use the
-- DescribeServices operation to get the category code for a service. Each
-- AWS service defines its own set of category codes.
createCase_categoryCode :: Lens.Lens' CreateCase (Core.Maybe Core.Text)
createCase_categoryCode = Lens.lens (\CreateCase' {categoryCode} -> categoryCode) (\s@CreateCase' {} a -> s {categoryCode = a} :: CreateCase)

-- | A list of email addresses that AWS Support copies on case
-- correspondence. AWS Support identifies the account that creates the case
-- when you specify your AWS credentials in an HTTP POST method or use the
-- <http://aws.amazon.com/tools/ AWS SDKs>.
createCase_ccEmailAddresses :: Lens.Lens' CreateCase (Core.Maybe [Core.Text])
createCase_ccEmailAddresses = Lens.lens (\CreateCase' {ccEmailAddresses} -> ccEmailAddresses) (\s@CreateCase' {} a -> s {ccEmailAddresses = a} :: CreateCase) Core.. Lens.mapping Lens._Coerce

-- | The type of issue for the case. You can specify @customer-service@ or
-- @technical@. If you don\'t specify a value, the default is @technical@.
createCase_issueType :: Lens.Lens' CreateCase (Core.Maybe Core.Text)
createCase_issueType = Lens.lens (\CreateCase' {issueType} -> issueType) (\s@CreateCase' {} a -> s {issueType = a} :: CreateCase)

-- | The ID of a set of one or more attachments for the case. Create the set
-- by using the AddAttachmentsToSet operation.
createCase_attachmentSetId :: Lens.Lens' CreateCase (Core.Maybe Core.Text)
createCase_attachmentSetId = Lens.lens (\CreateCase' {attachmentSetId} -> attachmentSetId) (\s@CreateCase' {} a -> s {attachmentSetId = a} :: CreateCase)

-- | A value that indicates the urgency of the case. This value determines
-- the response time according to your service level agreement with AWS
-- Support. You can use the DescribeSeverityLevels operation to get the
-- possible values for @severityCode@.
--
-- For more information, see SeverityLevel and
-- <https://docs.aws.amazon.com/awssupport/latest/user/getting-started.html#choosing-severity Choosing a Severity>
-- in the /AWS Support User Guide/.
--
-- The availability of severity levels depends on the support plan for the
-- AWS account.
createCase_severityCode :: Lens.Lens' CreateCase (Core.Maybe Core.Text)
createCase_severityCode = Lens.lens (\CreateCase' {severityCode} -> severityCode) (\s@CreateCase' {} a -> s {severityCode = a} :: CreateCase)

-- | The language in which AWS Support handles the case. You must specify the
-- ISO 639-1 code for the @language@ parameter if you want support in that
-- language. Currently, English (\"en\") and Japanese (\"ja\") are
-- supported.
createCase_language :: Lens.Lens' CreateCase (Core.Maybe Core.Text)
createCase_language = Lens.lens (\CreateCase' {language} -> language) (\s@CreateCase' {} a -> s {language = a} :: CreateCase)

-- | The title of the AWS Support case. The title appears in the __Subject__
-- field on the AWS Support Center
-- <https://console.aws.amazon.com/support/home#/case/create Create Case>
-- page.
createCase_subject :: Lens.Lens' CreateCase Core.Text
createCase_subject = Lens.lens (\CreateCase' {subject} -> subject) (\s@CreateCase' {} a -> s {subject = a} :: CreateCase)

-- | The communication body text that describes the issue. This text appears
-- in the __Description__ field on the AWS Support Center
-- <https://console.aws.amazon.com/support/home#/case/create Create Case>
-- page.
createCase_communicationBody :: Lens.Lens' CreateCase Core.Text
createCase_communicationBody = Lens.lens (\CreateCase' {communicationBody} -> communicationBody) (\s@CreateCase' {} a -> s {communicationBody = a} :: CreateCase)

instance Core.AWSRequest CreateCase where
  type AWSResponse CreateCase = CreateCaseResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateCaseResponse'
            Core.<$> (x Core..?> "caseId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateCase

instance Core.NFData CreateCase

instance Core.ToHeaders CreateCase where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSSupport_20130415.CreateCase" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateCase where
  toJSON CreateCase' {..} =
    Core.object
      ( Core.catMaybes
          [ ("serviceCode" Core..=) Core.<$> serviceCode,
            ("categoryCode" Core..=) Core.<$> categoryCode,
            ("ccEmailAddresses" Core..=)
              Core.<$> ccEmailAddresses,
            ("issueType" Core..=) Core.<$> issueType,
            ("attachmentSetId" Core..=) Core.<$> attachmentSetId,
            ("severityCode" Core..=) Core.<$> severityCode,
            ("language" Core..=) Core.<$> language,
            Core.Just ("subject" Core..= subject),
            Core.Just
              ("communicationBody" Core..= communicationBody)
          ]
      )

instance Core.ToPath CreateCase where
  toPath = Core.const "/"

instance Core.ToQuery CreateCase where
  toQuery = Core.const Core.mempty

-- | The AWS Support case ID returned by a successful completion of the
-- CreateCase operation.
--
-- /See:/ 'newCreateCaseResponse' smart constructor.
data CreateCaseResponse = CreateCaseResponse'
  { -- | The AWS Support case ID requested or returned in the call. The case ID
    -- is an alphanumeric string in the following format:
    -- case-/12345678910-2013-c4c1d2bf33c5cf47/
    caseId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateCaseResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'caseId', 'createCaseResponse_caseId' - The AWS Support case ID requested or returned in the call. The case ID
-- is an alphanumeric string in the following format:
-- case-/12345678910-2013-c4c1d2bf33c5cf47/
--
-- 'httpStatus', 'createCaseResponse_httpStatus' - The response's http status code.
newCreateCaseResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateCaseResponse
newCreateCaseResponse pHttpStatus_ =
  CreateCaseResponse'
    { caseId = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The AWS Support case ID requested or returned in the call. The case ID
-- is an alphanumeric string in the following format:
-- case-/12345678910-2013-c4c1d2bf33c5cf47/
createCaseResponse_caseId :: Lens.Lens' CreateCaseResponse (Core.Maybe Core.Text)
createCaseResponse_caseId = Lens.lens (\CreateCaseResponse' {caseId} -> caseId) (\s@CreateCaseResponse' {} a -> s {caseId = a} :: CreateCaseResponse)

-- | The response's http status code.
createCaseResponse_httpStatus :: Lens.Lens' CreateCaseResponse Core.Int
createCaseResponse_httpStatus = Lens.lens (\CreateCaseResponse' {httpStatus} -> httpStatus) (\s@CreateCaseResponse' {} a -> s {httpStatus = a} :: CreateCaseResponse)

instance Core.NFData CreateCaseResponse

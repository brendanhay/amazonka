{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Support.CreateCase
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a case in the AWS Support Center. This operation is similar to how you create a case in the AWS Support Center <https://console.aws.amazon.com/support/home#/case/create Create Case> page.
--
-- The AWS Support API doesn't support requesting service limit increases. You can submit a service limit increase in the following ways:
--
--     * Submit a request from the AWS Support Center <https://console.aws.amazon.com/support/home#/case/create Create Case> page.
--
--
--     * Use the Service Quotas <https://docs.aws.amazon.com/servicequotas/2019-06-24/apireference/API_RequestServiceQuotaIncrease.html RequestServiceQuotaIncrease> operation.
--
--
-- A successful @CreateCase@ request returns an AWS Support case number. You can use the 'DescribeCases' operation and specify the case number to get existing AWS Support cases. After you create a case, use the 'AddCommunicationToCase' operation to add additional communication or attachments to an existing case.
-- The @caseId@ is separate from the @displayId@ that appears in the <https://console.aws.amazon.com/support AWS Support Center> . Use the 'DescribeCases' operation to get the @displayId@ .
module Network.AWS.Support.CreateCase
  ( -- * Creating a request
    CreateCase (..),
    mkCreateCase,

    -- ** Request lenses
    ccSubject,
    ccCommunicationBody,
    ccSeverityCode,
    ccIssueType,
    ccCcEmailAddresses,
    ccLanguage,
    ccCategoryCode,
    ccServiceCode,
    ccAttachmentSetId,

    -- * Destructuring the response
    CreateCaseResponse (..),
    mkCreateCaseResponse,

    -- ** Response lenses
    ccrsCaseId,
    ccrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Support.Types

-- | /See:/ 'mkCreateCase' smart constructor.
data CreateCase = CreateCase'
  { -- | The title of the AWS Support case. The title appears in the __Subject__ field on the AWS Support Center <https://console.aws.amazon.com/support/home#/case/create Create Case> page.
    subject :: Lude.Text,
    -- | The communication body text that describes the issue. This text appears in the __Description__ field on the AWS Support Center <https://console.aws.amazon.com/support/home#/case/create Create Case> page.
    communicationBody :: Lude.Text,
    -- | A value that indicates the urgency of the case. This value determines the response time according to your service level agreement with AWS Support. You can use the 'DescribeSeverityLevels' operation to get the possible values for @severityCode@ .
    --
    -- For more information, see 'SeverityLevel' and <https://docs.aws.amazon.com/awssupport/latest/user/getting-started.html#choosing-severity Choosing a Severity> in the /AWS Support User Guide/ .
    severityCode :: Lude.Maybe Lude.Text,
    -- | The type of issue for the case. You can specify @customer-service@ or @technical@ . If you don't specify a value, the default is @technical@ .
    issueType :: Lude.Maybe Lude.Text,
    -- | A list of email addresses that AWS Support copies on case correspondence. AWS Support identifies the account that creates the case when you specify your AWS credentials in an HTTP POST method or use the <http://aws.amazon.com/tools/ AWS SDKs> .
    ccEmailAddresses :: Lude.Maybe [Lude.Text],
    -- | The language in which AWS Support handles the case. You must specify the ISO 639-1 code for the @language@ parameter if you want support in that language. Currently, English ("en") and Japanese ("ja") are supported.
    language :: Lude.Maybe Lude.Text,
    -- | The category of problem for the AWS Support case. You also use the 'DescribeServices' operation to get the category code for a service. Each AWS service defines its own set of category codes.
    categoryCode :: Lude.Maybe Lude.Text,
    -- | The code for the AWS service. You can use the 'DescribeServices' operation to get the possible @serviceCode@ values.
    serviceCode :: Lude.Maybe Lude.Text,
    -- | The ID of a set of one or more attachments for the case. Create the set by using the 'AddAttachmentsToSet' operation.
    attachmentSetId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateCase' with the minimum fields required to make a request.
--
-- * 'subject' - The title of the AWS Support case. The title appears in the __Subject__ field on the AWS Support Center <https://console.aws.amazon.com/support/home#/case/create Create Case> page.
-- * 'communicationBody' - The communication body text that describes the issue. This text appears in the __Description__ field on the AWS Support Center <https://console.aws.amazon.com/support/home#/case/create Create Case> page.
-- * 'severityCode' - A value that indicates the urgency of the case. This value determines the response time according to your service level agreement with AWS Support. You can use the 'DescribeSeverityLevels' operation to get the possible values for @severityCode@ .
--
-- For more information, see 'SeverityLevel' and <https://docs.aws.amazon.com/awssupport/latest/user/getting-started.html#choosing-severity Choosing a Severity> in the /AWS Support User Guide/ .
-- * 'issueType' - The type of issue for the case. You can specify @customer-service@ or @technical@ . If you don't specify a value, the default is @technical@ .
-- * 'ccEmailAddresses' - A list of email addresses that AWS Support copies on case correspondence. AWS Support identifies the account that creates the case when you specify your AWS credentials in an HTTP POST method or use the <http://aws.amazon.com/tools/ AWS SDKs> .
-- * 'language' - The language in which AWS Support handles the case. You must specify the ISO 639-1 code for the @language@ parameter if you want support in that language. Currently, English ("en") and Japanese ("ja") are supported.
-- * 'categoryCode' - The category of problem for the AWS Support case. You also use the 'DescribeServices' operation to get the category code for a service. Each AWS service defines its own set of category codes.
-- * 'serviceCode' - The code for the AWS service. You can use the 'DescribeServices' operation to get the possible @serviceCode@ values.
-- * 'attachmentSetId' - The ID of a set of one or more attachments for the case. Create the set by using the 'AddAttachmentsToSet' operation.
mkCreateCase ::
  -- | 'subject'
  Lude.Text ->
  -- | 'communicationBody'
  Lude.Text ->
  CreateCase
mkCreateCase pSubject_ pCommunicationBody_ =
  CreateCase'
    { subject = pSubject_,
      communicationBody = pCommunicationBody_,
      severityCode = Lude.Nothing,
      issueType = Lude.Nothing,
      ccEmailAddresses = Lude.Nothing,
      language = Lude.Nothing,
      categoryCode = Lude.Nothing,
      serviceCode = Lude.Nothing,
      attachmentSetId = Lude.Nothing
    }

-- | The title of the AWS Support case. The title appears in the __Subject__ field on the AWS Support Center <https://console.aws.amazon.com/support/home#/case/create Create Case> page.
--
-- /Note:/ Consider using 'subject' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccSubject :: Lens.Lens' CreateCase Lude.Text
ccSubject = Lens.lens (subject :: CreateCase -> Lude.Text) (\s a -> s {subject = a} :: CreateCase)
{-# DEPRECATED ccSubject "Use generic-lens or generic-optics with 'subject' instead." #-}

-- | The communication body text that describes the issue. This text appears in the __Description__ field on the AWS Support Center <https://console.aws.amazon.com/support/home#/case/create Create Case> page.
--
-- /Note:/ Consider using 'communicationBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccCommunicationBody :: Lens.Lens' CreateCase Lude.Text
ccCommunicationBody = Lens.lens (communicationBody :: CreateCase -> Lude.Text) (\s a -> s {communicationBody = a} :: CreateCase)
{-# DEPRECATED ccCommunicationBody "Use generic-lens or generic-optics with 'communicationBody' instead." #-}

-- | A value that indicates the urgency of the case. This value determines the response time according to your service level agreement with AWS Support. You can use the 'DescribeSeverityLevels' operation to get the possible values for @severityCode@ .
--
-- For more information, see 'SeverityLevel' and <https://docs.aws.amazon.com/awssupport/latest/user/getting-started.html#choosing-severity Choosing a Severity> in the /AWS Support User Guide/ .
--
-- /Note:/ Consider using 'severityCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccSeverityCode :: Lens.Lens' CreateCase (Lude.Maybe Lude.Text)
ccSeverityCode = Lens.lens (severityCode :: CreateCase -> Lude.Maybe Lude.Text) (\s a -> s {severityCode = a} :: CreateCase)
{-# DEPRECATED ccSeverityCode "Use generic-lens or generic-optics with 'severityCode' instead." #-}

-- | The type of issue for the case. You can specify @customer-service@ or @technical@ . If you don't specify a value, the default is @technical@ .
--
-- /Note:/ Consider using 'issueType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccIssueType :: Lens.Lens' CreateCase (Lude.Maybe Lude.Text)
ccIssueType = Lens.lens (issueType :: CreateCase -> Lude.Maybe Lude.Text) (\s a -> s {issueType = a} :: CreateCase)
{-# DEPRECATED ccIssueType "Use generic-lens or generic-optics with 'issueType' instead." #-}

-- | A list of email addresses that AWS Support copies on case correspondence. AWS Support identifies the account that creates the case when you specify your AWS credentials in an HTTP POST method or use the <http://aws.amazon.com/tools/ AWS SDKs> .
--
-- /Note:/ Consider using 'ccEmailAddresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccCcEmailAddresses :: Lens.Lens' CreateCase (Lude.Maybe [Lude.Text])
ccCcEmailAddresses = Lens.lens (ccEmailAddresses :: CreateCase -> Lude.Maybe [Lude.Text]) (\s a -> s {ccEmailAddresses = a} :: CreateCase)
{-# DEPRECATED ccCcEmailAddresses "Use generic-lens or generic-optics with 'ccEmailAddresses' instead." #-}

-- | The language in which AWS Support handles the case. You must specify the ISO 639-1 code for the @language@ parameter if you want support in that language. Currently, English ("en") and Japanese ("ja") are supported.
--
-- /Note:/ Consider using 'language' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccLanguage :: Lens.Lens' CreateCase (Lude.Maybe Lude.Text)
ccLanguage = Lens.lens (language :: CreateCase -> Lude.Maybe Lude.Text) (\s a -> s {language = a} :: CreateCase)
{-# DEPRECATED ccLanguage "Use generic-lens or generic-optics with 'language' instead." #-}

-- | The category of problem for the AWS Support case. You also use the 'DescribeServices' operation to get the category code for a service. Each AWS service defines its own set of category codes.
--
-- /Note:/ Consider using 'categoryCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccCategoryCode :: Lens.Lens' CreateCase (Lude.Maybe Lude.Text)
ccCategoryCode = Lens.lens (categoryCode :: CreateCase -> Lude.Maybe Lude.Text) (\s a -> s {categoryCode = a} :: CreateCase)
{-# DEPRECATED ccCategoryCode "Use generic-lens or generic-optics with 'categoryCode' instead." #-}

-- | The code for the AWS service. You can use the 'DescribeServices' operation to get the possible @serviceCode@ values.
--
-- /Note:/ Consider using 'serviceCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccServiceCode :: Lens.Lens' CreateCase (Lude.Maybe Lude.Text)
ccServiceCode = Lens.lens (serviceCode :: CreateCase -> Lude.Maybe Lude.Text) (\s a -> s {serviceCode = a} :: CreateCase)
{-# DEPRECATED ccServiceCode "Use generic-lens or generic-optics with 'serviceCode' instead." #-}

-- | The ID of a set of one or more attachments for the case. Create the set by using the 'AddAttachmentsToSet' operation.
--
-- /Note:/ Consider using 'attachmentSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccAttachmentSetId :: Lens.Lens' CreateCase (Lude.Maybe Lude.Text)
ccAttachmentSetId = Lens.lens (attachmentSetId :: CreateCase -> Lude.Maybe Lude.Text) (\s a -> s {attachmentSetId = a} :: CreateCase)
{-# DEPRECATED ccAttachmentSetId "Use generic-lens or generic-optics with 'attachmentSetId' instead." #-}

instance Lude.AWSRequest CreateCase where
  type Rs CreateCase = CreateCaseResponse
  request = Req.postJSON supportService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateCaseResponse'
            Lude.<$> (x Lude..?> "caseId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateCase where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSSupport_20130415.CreateCase" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateCase where
  toJSON CreateCase' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("subject" Lude..= subject),
            Lude.Just ("communicationBody" Lude..= communicationBody),
            ("severityCode" Lude..=) Lude.<$> severityCode,
            ("issueType" Lude..=) Lude.<$> issueType,
            ("ccEmailAddresses" Lude..=) Lude.<$> ccEmailAddresses,
            ("language" Lude..=) Lude.<$> language,
            ("categoryCode" Lude..=) Lude.<$> categoryCode,
            ("serviceCode" Lude..=) Lude.<$> serviceCode,
            ("attachmentSetId" Lude..=) Lude.<$> attachmentSetId
          ]
      )

instance Lude.ToPath CreateCase where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateCase where
  toQuery = Lude.const Lude.mempty

-- | The AWS Support case ID returned by a successful completion of the 'CreateCase' operation.
--
-- /See:/ 'mkCreateCaseResponse' smart constructor.
data CreateCaseResponse = CreateCaseResponse'
  { -- | The AWS Support case ID requested or returned in the call. The case ID is an alphanumeric string in the following format: case-/12345678910-2013-c4c1d2bf33c5cf47/
    caseId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateCaseResponse' with the minimum fields required to make a request.
--
-- * 'caseId' - The AWS Support case ID requested or returned in the call. The case ID is an alphanumeric string in the following format: case-/12345678910-2013-c4c1d2bf33c5cf47/
-- * 'responseStatus' - The response status code.
mkCreateCaseResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateCaseResponse
mkCreateCaseResponse pResponseStatus_ =
  CreateCaseResponse'
    { caseId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The AWS Support case ID requested or returned in the call. The case ID is an alphanumeric string in the following format: case-/12345678910-2013-c4c1d2bf33c5cf47/
--
-- /Note:/ Consider using 'caseId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrsCaseId :: Lens.Lens' CreateCaseResponse (Lude.Maybe Lude.Text)
ccrsCaseId = Lens.lens (caseId :: CreateCaseResponse -> Lude.Maybe Lude.Text) (\s a -> s {caseId = a} :: CreateCaseResponse)
{-# DEPRECATED ccrsCaseId "Use generic-lens or generic-optics with 'caseId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrsResponseStatus :: Lens.Lens' CreateCaseResponse Lude.Int
ccrsResponseStatus = Lens.lens (responseStatus :: CreateCaseResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateCaseResponse)
{-# DEPRECATED ccrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CreateCase (..)
    , mkCreateCase
    -- ** Request lenses
    , ccSubject
    , ccCommunicationBody
    , ccAttachmentSetId
    , ccCategoryCode
    , ccCcEmailAddresses
    , ccIssueType
    , ccLanguage
    , ccServiceCode
    , ccSeverityCode

    -- * Destructuring the response
    , CreateCaseResponse (..)
    , mkCreateCaseResponse
    -- ** Response lenses
    , ccrrsCaseId
    , ccrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Support.Types as Types

-- | /See:/ 'mkCreateCase' smart constructor.
data CreateCase = CreateCase'
  { subject :: Types.Subject
    -- ^ The title of the AWS Support case. The title appears in the __Subject__ field on the AWS Support Center <https://console.aws.amazon.com/support/home#/case/create Create Case> page.
  , communicationBody :: Types.CommunicationBody
    -- ^ The communication body text that describes the issue. This text appears in the __Description__ field on the AWS Support Center <https://console.aws.amazon.com/support/home#/case/create Create Case> page.
  , attachmentSetId :: Core.Maybe Types.AttachmentSetId
    -- ^ The ID of a set of one or more attachments for the case. Create the set by using the 'AddAttachmentsToSet' operation.
  , categoryCode :: Core.Maybe Types.CategoryCode
    -- ^ The category of problem for the AWS Support case. You also use the 'DescribeServices' operation to get the category code for a service. Each AWS service defines its own set of category codes.
  , ccEmailAddresses :: Core.Maybe [Types.CcEmailAddress]
    -- ^ A list of email addresses that AWS Support copies on case correspondence. AWS Support identifies the account that creates the case when you specify your AWS credentials in an HTTP POST method or use the <http://aws.amazon.com/tools/ AWS SDKs> . 
  , issueType :: Core.Maybe Types.IssueType
    -- ^ The type of issue for the case. You can specify @customer-service@ or @technical@ . If you don't specify a value, the default is @technical@ .
  , language :: Core.Maybe Types.Language
    -- ^ The language in which AWS Support handles the case. You must specify the ISO 639-1 code for the @language@ parameter if you want support in that language. Currently, English ("en") and Japanese ("ja") are supported.
  , serviceCode :: Core.Maybe Types.ServiceCode
    -- ^ The code for the AWS service. You can use the 'DescribeServices' operation to get the possible @serviceCode@ values.
  , severityCode :: Core.Maybe Types.SeverityCode
    -- ^ A value that indicates the urgency of the case. This value determines the response time according to your service level agreement with AWS Support. You can use the 'DescribeSeverityLevels' operation to get the possible values for @severityCode@ . 
--
-- For more information, see 'SeverityLevel' and <https://docs.aws.amazon.com/awssupport/latest/user/getting-started.html#choosing-severity Choosing a Severity> in the /AWS Support User Guide/ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateCase' value with any optional fields omitted.
mkCreateCase
    :: Types.Subject -- ^ 'subject'
    -> Types.CommunicationBody -- ^ 'communicationBody'
    -> CreateCase
mkCreateCase subject communicationBody
  = CreateCase'{subject, communicationBody,
                attachmentSetId = Core.Nothing, categoryCode = Core.Nothing,
                ccEmailAddresses = Core.Nothing, issueType = Core.Nothing,
                language = Core.Nothing, serviceCode = Core.Nothing,
                severityCode = Core.Nothing}

-- | The title of the AWS Support case. The title appears in the __Subject__ field on the AWS Support Center <https://console.aws.amazon.com/support/home#/case/create Create Case> page.
--
-- /Note:/ Consider using 'subject' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccSubject :: Lens.Lens' CreateCase Types.Subject
ccSubject = Lens.field @"subject"
{-# INLINEABLE ccSubject #-}
{-# DEPRECATED subject "Use generic-lens or generic-optics with 'subject' instead"  #-}

-- | The communication body text that describes the issue. This text appears in the __Description__ field on the AWS Support Center <https://console.aws.amazon.com/support/home#/case/create Create Case> page.
--
-- /Note:/ Consider using 'communicationBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccCommunicationBody :: Lens.Lens' CreateCase Types.CommunicationBody
ccCommunicationBody = Lens.field @"communicationBody"
{-# INLINEABLE ccCommunicationBody #-}
{-# DEPRECATED communicationBody "Use generic-lens or generic-optics with 'communicationBody' instead"  #-}

-- | The ID of a set of one or more attachments for the case. Create the set by using the 'AddAttachmentsToSet' operation.
--
-- /Note:/ Consider using 'attachmentSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccAttachmentSetId :: Lens.Lens' CreateCase (Core.Maybe Types.AttachmentSetId)
ccAttachmentSetId = Lens.field @"attachmentSetId"
{-# INLINEABLE ccAttachmentSetId #-}
{-# DEPRECATED attachmentSetId "Use generic-lens or generic-optics with 'attachmentSetId' instead"  #-}

-- | The category of problem for the AWS Support case. You also use the 'DescribeServices' operation to get the category code for a service. Each AWS service defines its own set of category codes.
--
-- /Note:/ Consider using 'categoryCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccCategoryCode :: Lens.Lens' CreateCase (Core.Maybe Types.CategoryCode)
ccCategoryCode = Lens.field @"categoryCode"
{-# INLINEABLE ccCategoryCode #-}
{-# DEPRECATED categoryCode "Use generic-lens or generic-optics with 'categoryCode' instead"  #-}

-- | A list of email addresses that AWS Support copies on case correspondence. AWS Support identifies the account that creates the case when you specify your AWS credentials in an HTTP POST method or use the <http://aws.amazon.com/tools/ AWS SDKs> . 
--
-- /Note:/ Consider using 'ccEmailAddresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccCcEmailAddresses :: Lens.Lens' CreateCase (Core.Maybe [Types.CcEmailAddress])
ccCcEmailAddresses = Lens.field @"ccEmailAddresses"
{-# INLINEABLE ccCcEmailAddresses #-}
{-# DEPRECATED ccEmailAddresses "Use generic-lens or generic-optics with 'ccEmailAddresses' instead"  #-}

-- | The type of issue for the case. You can specify @customer-service@ or @technical@ . If you don't specify a value, the default is @technical@ .
--
-- /Note:/ Consider using 'issueType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccIssueType :: Lens.Lens' CreateCase (Core.Maybe Types.IssueType)
ccIssueType = Lens.field @"issueType"
{-# INLINEABLE ccIssueType #-}
{-# DEPRECATED issueType "Use generic-lens or generic-optics with 'issueType' instead"  #-}

-- | The language in which AWS Support handles the case. You must specify the ISO 639-1 code for the @language@ parameter if you want support in that language. Currently, English ("en") and Japanese ("ja") are supported.
--
-- /Note:/ Consider using 'language' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccLanguage :: Lens.Lens' CreateCase (Core.Maybe Types.Language)
ccLanguage = Lens.field @"language"
{-# INLINEABLE ccLanguage #-}
{-# DEPRECATED language "Use generic-lens or generic-optics with 'language' instead"  #-}

-- | The code for the AWS service. You can use the 'DescribeServices' operation to get the possible @serviceCode@ values.
--
-- /Note:/ Consider using 'serviceCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccServiceCode :: Lens.Lens' CreateCase (Core.Maybe Types.ServiceCode)
ccServiceCode = Lens.field @"serviceCode"
{-# INLINEABLE ccServiceCode #-}
{-# DEPRECATED serviceCode "Use generic-lens or generic-optics with 'serviceCode' instead"  #-}

-- | A value that indicates the urgency of the case. This value determines the response time according to your service level agreement with AWS Support. You can use the 'DescribeSeverityLevels' operation to get the possible values for @severityCode@ . 
--
-- For more information, see 'SeverityLevel' and <https://docs.aws.amazon.com/awssupport/latest/user/getting-started.html#choosing-severity Choosing a Severity> in the /AWS Support User Guide/ .
--
-- /Note:/ Consider using 'severityCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccSeverityCode :: Lens.Lens' CreateCase (Core.Maybe Types.SeverityCode)
ccSeverityCode = Lens.field @"severityCode"
{-# INLINEABLE ccSeverityCode #-}
{-# DEPRECATED severityCode "Use generic-lens or generic-optics with 'severityCode' instead"  #-}

instance Core.ToQuery CreateCase where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateCase where
        toHeaders CreateCase{..}
          = Core.pure ("X-Amz-Target", "AWSSupport_20130415.CreateCase")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateCase where
        toJSON CreateCase{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("subject" Core..= subject),
                  Core.Just ("communicationBody" Core..= communicationBody),
                  ("attachmentSetId" Core..=) Core.<$> attachmentSetId,
                  ("categoryCode" Core..=) Core.<$> categoryCode,
                  ("ccEmailAddresses" Core..=) Core.<$> ccEmailAddresses,
                  ("issueType" Core..=) Core.<$> issueType,
                  ("language" Core..=) Core.<$> language,
                  ("serviceCode" Core..=) Core.<$> serviceCode,
                  ("severityCode" Core..=) Core.<$> severityCode])

instance Core.AWSRequest CreateCase where
        type Rs CreateCase = CreateCaseResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateCaseResponse' Core.<$>
                   (x Core..:? "caseId") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The AWS Support case ID returned by a successful completion of the 'CreateCase' operation.
--
-- /See:/ 'mkCreateCaseResponse' smart constructor.
data CreateCaseResponse = CreateCaseResponse'
  { caseId :: Core.Maybe Types.CaseId
    -- ^ The AWS Support case ID requested or returned in the call. The case ID is an alphanumeric string in the following format: case-/12345678910-2013-c4c1d2bf33c5cf47/ 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateCaseResponse' value with any optional fields omitted.
mkCreateCaseResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateCaseResponse
mkCreateCaseResponse responseStatus
  = CreateCaseResponse'{caseId = Core.Nothing, responseStatus}

-- | The AWS Support case ID requested or returned in the call. The case ID is an alphanumeric string in the following format: case-/12345678910-2013-c4c1d2bf33c5cf47/ 
--
-- /Note:/ Consider using 'caseId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrrsCaseId :: Lens.Lens' CreateCaseResponse (Core.Maybe Types.CaseId)
ccrrsCaseId = Lens.field @"caseId"
{-# INLINEABLE ccrrsCaseId #-}
{-# DEPRECATED caseId "Use generic-lens or generic-optics with 'caseId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrrsResponseStatus :: Lens.Lens' CreateCaseResponse Core.Int
ccrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ccrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

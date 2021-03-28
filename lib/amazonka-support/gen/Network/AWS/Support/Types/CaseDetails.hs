{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Support.Types.CaseDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Support.Types.CaseDetails
  ( CaseDetails (..)
  -- * Smart constructor
  , mkCaseDetails
  -- * Lenses
  , cdCaseId
  , cdCategoryCode
  , cdCcEmailAddresses
  , cdDisplayId
  , cdLanguage
  , cdRecentCommunications
  , cdServiceCode
  , cdSeverityCode
  , cdStatus
  , cdSubject
  , cdSubmittedBy
  , cdTimeCreated
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Support.Types.CaseId as Types
import qualified Network.AWS.Support.Types.CategoryCode as Types
import qualified Network.AWS.Support.Types.CcEmailAddress as Types
import qualified Network.AWS.Support.Types.DisplayId as Types
import qualified Network.AWS.Support.Types.Language as Types
import qualified Network.AWS.Support.Types.RecentCaseCommunications as Types
import qualified Network.AWS.Support.Types.ServiceCode as Types
import qualified Network.AWS.Support.Types.SeverityCode as Types
import qualified Network.AWS.Support.Types.Status as Types
import qualified Network.AWS.Support.Types.Subject as Types
import qualified Network.AWS.Support.Types.SubmittedBy as Types
import qualified Network.AWS.Support.Types.TimeCreated as Types

-- | A JSON-formatted object that contains the metadata for a support case. It is contained in the response from a 'DescribeCases' request. __CaseDetails__ contains the following fields:
--
--
--     * __caseId.__ The AWS Support case ID requested or returned in the call. The case ID is an alphanumeric string formatted as shown in this example: case-/12345678910-2013-c4c1d2bf33c5cf47/ .
--
--
--     * __categoryCode.__ The category of problem for the AWS Support case. Corresponds to the CategoryCode values returned by a call to 'DescribeServices' .
--
--
--     * __displayId.__ The identifier for the case on pages in the AWS Support Center.
--
--
--     * __language.__ The ISO 639-1 code for the language in which AWS provides support. AWS Support currently supports English ("en") and Japanese ("ja"). Language parameters must be passed explicitly for operations that take them.
--
--
--     * __nextToken.__ A resumption point for pagination.
--
--
--     * __recentCommunications.__ One or more 'Communication' objects. Fields of these objects are @attachments@ , @body@ , @caseId@ , @submittedBy@ , and @timeCreated@ .
--
--
--     * __serviceCode.__ The identifier for the AWS service that corresponds to the service code defined in the call to 'DescribeServices' .
--
--
--     * __severityCode.__ The severity code assigned to the case. Contains one of the values returned by the call to 'DescribeSeverityLevels' . The possible values are: @low@ , @normal@ , @high@ , @urgent@ , and @critical@ .
--
--
--     * __status.__ The status of the case in the AWS Support Center. Valid values:
--
--     * @opened@ 
--
--
--     * @pending-customer-action@ 
--
--
--     * @reopened@ 
--
--
--     * @resolved@ 
--
--
--     * @unassigned@ 
--
--
--     * @work-in-progress@ 
--
--
--
--
--     * __subject.__ The subject line of the case.
--
--
--     * __submittedBy.__ The email address of the account that submitted the case.
--
--
--     * __timeCreated.__ The time the case was created, in ISO-8601 format.
--
--
--
-- /See:/ 'mkCaseDetails' smart constructor.
data CaseDetails = CaseDetails'
  { caseId :: Core.Maybe Types.CaseId
    -- ^ The AWS Support case ID requested or returned in the call. The case ID is an alphanumeric string formatted as shown in this example: case-/12345678910-2013-c4c1d2bf33c5cf47/ 
  , categoryCode :: Core.Maybe Types.CategoryCode
    -- ^ The category of problem for the AWS Support case.
  , ccEmailAddresses :: Core.Maybe [Types.CcEmailAddress]
    -- ^ The email addresses that receive copies of communication about the case.
  , displayId :: Core.Maybe Types.DisplayId
    -- ^ The ID displayed for the case in the AWS Support Center. This is a numeric string.
  , language :: Core.Maybe Types.Language
    -- ^ The ISO 639-1 code for the language in which AWS provides support. AWS Support currently supports English ("en") and Japanese ("ja"). Language parameters must be passed explicitly for operations that take them.
  , recentCommunications :: Core.Maybe Types.RecentCaseCommunications
    -- ^ The five most recent communications between you and AWS Support Center, including the IDs of any attachments to the communications. Also includes a @nextToken@ that you can use to retrieve earlier communications.
  , serviceCode :: Core.Maybe Types.ServiceCode
    -- ^ The code for the AWS service. You can get a list of codes and the corresponding service names by calling 'DescribeServices' .
  , severityCode :: Core.Maybe Types.SeverityCode
    -- ^ The code for the severity level returned by the call to 'DescribeSeverityLevels' .
  , status :: Core.Maybe Types.Status
    -- ^ The status of the case.
--
-- Valid values:
--
--     * @opened@ 
--
--
--     * @pending-customer-action@ 
--
--
--     * @reopened@ 
--
--
--     * @resolved@ 
--
--
--     * @unassigned@ 
--
--
--     * @work-in-progress@ 
--
--
  , subject :: Core.Maybe Types.Subject
    -- ^ The subject line for the case in the AWS Support Center.
  , submittedBy :: Core.Maybe Types.SubmittedBy
    -- ^ The email address of the account that submitted the case.
  , timeCreated :: Core.Maybe Types.TimeCreated
    -- ^ The time that the case was created in the AWS Support Center.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CaseDetails' value with any optional fields omitted.
mkCaseDetails
    :: CaseDetails
mkCaseDetails
  = CaseDetails'{caseId = Core.Nothing, categoryCode = Core.Nothing,
                 ccEmailAddresses = Core.Nothing, displayId = Core.Nothing,
                 language = Core.Nothing, recentCommunications = Core.Nothing,
                 serviceCode = Core.Nothing, severityCode = Core.Nothing,
                 status = Core.Nothing, subject = Core.Nothing,
                 submittedBy = Core.Nothing, timeCreated = Core.Nothing}

-- | The AWS Support case ID requested or returned in the call. The case ID is an alphanumeric string formatted as shown in this example: case-/12345678910-2013-c4c1d2bf33c5cf47/ 
--
-- /Note:/ Consider using 'caseId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdCaseId :: Lens.Lens' CaseDetails (Core.Maybe Types.CaseId)
cdCaseId = Lens.field @"caseId"
{-# INLINEABLE cdCaseId #-}
{-# DEPRECATED caseId "Use generic-lens or generic-optics with 'caseId' instead"  #-}

-- | The category of problem for the AWS Support case.
--
-- /Note:/ Consider using 'categoryCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdCategoryCode :: Lens.Lens' CaseDetails (Core.Maybe Types.CategoryCode)
cdCategoryCode = Lens.field @"categoryCode"
{-# INLINEABLE cdCategoryCode #-}
{-# DEPRECATED categoryCode "Use generic-lens or generic-optics with 'categoryCode' instead"  #-}

-- | The email addresses that receive copies of communication about the case.
--
-- /Note:/ Consider using 'ccEmailAddresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdCcEmailAddresses :: Lens.Lens' CaseDetails (Core.Maybe [Types.CcEmailAddress])
cdCcEmailAddresses = Lens.field @"ccEmailAddresses"
{-# INLINEABLE cdCcEmailAddresses #-}
{-# DEPRECATED ccEmailAddresses "Use generic-lens or generic-optics with 'ccEmailAddresses' instead"  #-}

-- | The ID displayed for the case in the AWS Support Center. This is a numeric string.
--
-- /Note:/ Consider using 'displayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdDisplayId :: Lens.Lens' CaseDetails (Core.Maybe Types.DisplayId)
cdDisplayId = Lens.field @"displayId"
{-# INLINEABLE cdDisplayId #-}
{-# DEPRECATED displayId "Use generic-lens or generic-optics with 'displayId' instead"  #-}

-- | The ISO 639-1 code for the language in which AWS provides support. AWS Support currently supports English ("en") and Japanese ("ja"). Language parameters must be passed explicitly for operations that take them.
--
-- /Note:/ Consider using 'language' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdLanguage :: Lens.Lens' CaseDetails (Core.Maybe Types.Language)
cdLanguage = Lens.field @"language"
{-# INLINEABLE cdLanguage #-}
{-# DEPRECATED language "Use generic-lens or generic-optics with 'language' instead"  #-}

-- | The five most recent communications between you and AWS Support Center, including the IDs of any attachments to the communications. Also includes a @nextToken@ that you can use to retrieve earlier communications.
--
-- /Note:/ Consider using 'recentCommunications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdRecentCommunications :: Lens.Lens' CaseDetails (Core.Maybe Types.RecentCaseCommunications)
cdRecentCommunications = Lens.field @"recentCommunications"
{-# INLINEABLE cdRecentCommunications #-}
{-# DEPRECATED recentCommunications "Use generic-lens or generic-optics with 'recentCommunications' instead"  #-}

-- | The code for the AWS service. You can get a list of codes and the corresponding service names by calling 'DescribeServices' .
--
-- /Note:/ Consider using 'serviceCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdServiceCode :: Lens.Lens' CaseDetails (Core.Maybe Types.ServiceCode)
cdServiceCode = Lens.field @"serviceCode"
{-# INLINEABLE cdServiceCode #-}
{-# DEPRECATED serviceCode "Use generic-lens or generic-optics with 'serviceCode' instead"  #-}

-- | The code for the severity level returned by the call to 'DescribeSeverityLevels' .
--
-- /Note:/ Consider using 'severityCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdSeverityCode :: Lens.Lens' CaseDetails (Core.Maybe Types.SeverityCode)
cdSeverityCode = Lens.field @"severityCode"
{-# INLINEABLE cdSeverityCode #-}
{-# DEPRECATED severityCode "Use generic-lens or generic-optics with 'severityCode' instead"  #-}

-- | The status of the case.
--
-- Valid values:
--
--     * @opened@ 
--
--
--     * @pending-customer-action@ 
--
--
--     * @reopened@ 
--
--
--     * @resolved@ 
--
--
--     * @unassigned@ 
--
--
--     * @work-in-progress@ 
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdStatus :: Lens.Lens' CaseDetails (Core.Maybe Types.Status)
cdStatus = Lens.field @"status"
{-# INLINEABLE cdStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The subject line for the case in the AWS Support Center.
--
-- /Note:/ Consider using 'subject' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdSubject :: Lens.Lens' CaseDetails (Core.Maybe Types.Subject)
cdSubject = Lens.field @"subject"
{-# INLINEABLE cdSubject #-}
{-# DEPRECATED subject "Use generic-lens or generic-optics with 'subject' instead"  #-}

-- | The email address of the account that submitted the case.
--
-- /Note:/ Consider using 'submittedBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdSubmittedBy :: Lens.Lens' CaseDetails (Core.Maybe Types.SubmittedBy)
cdSubmittedBy = Lens.field @"submittedBy"
{-# INLINEABLE cdSubmittedBy #-}
{-# DEPRECATED submittedBy "Use generic-lens or generic-optics with 'submittedBy' instead"  #-}

-- | The time that the case was created in the AWS Support Center.
--
-- /Note:/ Consider using 'timeCreated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdTimeCreated :: Lens.Lens' CaseDetails (Core.Maybe Types.TimeCreated)
cdTimeCreated = Lens.field @"timeCreated"
{-# INLINEABLE cdTimeCreated #-}
{-# DEPRECATED timeCreated "Use generic-lens or generic-optics with 'timeCreated' instead"  #-}

instance Core.FromJSON CaseDetails where
        parseJSON
          = Core.withObject "CaseDetails" Core.$
              \ x ->
                CaseDetails' Core.<$>
                  (x Core..:? "caseId") Core.<*> x Core..:? "categoryCode" Core.<*>
                    x Core..:? "ccEmailAddresses"
                    Core.<*> x Core..:? "displayId"
                    Core.<*> x Core..:? "language"
                    Core.<*> x Core..:? "recentCommunications"
                    Core.<*> x Core..:? "serviceCode"
                    Core.<*> x Core..:? "severityCode"
                    Core.<*> x Core..:? "status"
                    Core.<*> x Core..:? "subject"
                    Core.<*> x Core..:? "submittedBy"
                    Core.<*> x Core..:? "timeCreated"

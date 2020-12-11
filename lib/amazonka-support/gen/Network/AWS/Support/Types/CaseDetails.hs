-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Support.Types.CaseDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Support.Types.CaseDetails
  ( CaseDetails (..),

    -- * Smart constructor
    mkCaseDetails,

    -- * Lenses
    cdSubject,
    cdStatus,
    cdRecentCommunications,
    cdSeverityCode,
    cdCaseId,
    cdCcEmailAddresses,
    cdDisplayId,
    cdSubmittedBy,
    cdLanguage,
    cdTimeCreated,
    cdCategoryCode,
    cdServiceCode,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Support.Types.RecentCaseCommunications

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
  { subject :: Lude.Maybe Lude.Text,
    status :: Lude.Maybe Lude.Text,
    recentCommunications :: Lude.Maybe RecentCaseCommunications,
    severityCode :: Lude.Maybe Lude.Text,
    caseId :: Lude.Maybe Lude.Text,
    ccEmailAddresses :: Lude.Maybe [Lude.Text],
    displayId :: Lude.Maybe Lude.Text,
    submittedBy :: Lude.Maybe Lude.Text,
    language :: Lude.Maybe Lude.Text,
    timeCreated :: Lude.Maybe Lude.Text,
    categoryCode :: Lude.Maybe Lude.Text,
    serviceCode :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CaseDetails' with the minimum fields required to make a request.
--
-- * 'caseId' - The AWS Support case ID requested or returned in the call. The case ID is an alphanumeric string formatted as shown in this example: case-/12345678910-2013-c4c1d2bf33c5cf47/
-- * 'categoryCode' - The category of problem for the AWS Support case.
-- * 'ccEmailAddresses' - The email addresses that receive copies of communication about the case.
-- * 'displayId' - The ID displayed for the case in the AWS Support Center. This is a numeric string.
-- * 'language' - The ISO 639-1 code for the language in which AWS provides support. AWS Support currently supports English ("en") and Japanese ("ja"). Language parameters must be passed explicitly for operations that take them.
-- * 'recentCommunications' - The five most recent communications between you and AWS Support Center, including the IDs of any attachments to the communications. Also includes a @nextToken@ that you can use to retrieve earlier communications.
-- * 'serviceCode' - The code for the AWS service. You can get a list of codes and the corresponding service names by calling 'DescribeServices' .
-- * 'severityCode' - The code for the severity level returned by the call to 'DescribeSeverityLevels' .
-- * 'status' - The status of the case.
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
-- * 'subject' - The subject line for the case in the AWS Support Center.
-- * 'submittedBy' - The email address of the account that submitted the case.
-- * 'timeCreated' - The time that the case was created in the AWS Support Center.
mkCaseDetails ::
  CaseDetails
mkCaseDetails =
  CaseDetails'
    { subject = Lude.Nothing,
      status = Lude.Nothing,
      recentCommunications = Lude.Nothing,
      severityCode = Lude.Nothing,
      caseId = Lude.Nothing,
      ccEmailAddresses = Lude.Nothing,
      displayId = Lude.Nothing,
      submittedBy = Lude.Nothing,
      language = Lude.Nothing,
      timeCreated = Lude.Nothing,
      categoryCode = Lude.Nothing,
      serviceCode = Lude.Nothing
    }

-- | The subject line for the case in the AWS Support Center.
--
-- /Note:/ Consider using 'subject' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdSubject :: Lens.Lens' CaseDetails (Lude.Maybe Lude.Text)
cdSubject = Lens.lens (subject :: CaseDetails -> Lude.Maybe Lude.Text) (\s a -> s {subject = a} :: CaseDetails)
{-# DEPRECATED cdSubject "Use generic-lens or generic-optics with 'subject' instead." #-}

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
cdStatus :: Lens.Lens' CaseDetails (Lude.Maybe Lude.Text)
cdStatus = Lens.lens (status :: CaseDetails -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: CaseDetails)
{-# DEPRECATED cdStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The five most recent communications between you and AWS Support Center, including the IDs of any attachments to the communications. Also includes a @nextToken@ that you can use to retrieve earlier communications.
--
-- /Note:/ Consider using 'recentCommunications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdRecentCommunications :: Lens.Lens' CaseDetails (Lude.Maybe RecentCaseCommunications)
cdRecentCommunications = Lens.lens (recentCommunications :: CaseDetails -> Lude.Maybe RecentCaseCommunications) (\s a -> s {recentCommunications = a} :: CaseDetails)
{-# DEPRECATED cdRecentCommunications "Use generic-lens or generic-optics with 'recentCommunications' instead." #-}

-- | The code for the severity level returned by the call to 'DescribeSeverityLevels' .
--
-- /Note:/ Consider using 'severityCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdSeverityCode :: Lens.Lens' CaseDetails (Lude.Maybe Lude.Text)
cdSeverityCode = Lens.lens (severityCode :: CaseDetails -> Lude.Maybe Lude.Text) (\s a -> s {severityCode = a} :: CaseDetails)
{-# DEPRECATED cdSeverityCode "Use generic-lens or generic-optics with 'severityCode' instead." #-}

-- | The AWS Support case ID requested or returned in the call. The case ID is an alphanumeric string formatted as shown in this example: case-/12345678910-2013-c4c1d2bf33c5cf47/
--
-- /Note:/ Consider using 'caseId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdCaseId :: Lens.Lens' CaseDetails (Lude.Maybe Lude.Text)
cdCaseId = Lens.lens (caseId :: CaseDetails -> Lude.Maybe Lude.Text) (\s a -> s {caseId = a} :: CaseDetails)
{-# DEPRECATED cdCaseId "Use generic-lens or generic-optics with 'caseId' instead." #-}

-- | The email addresses that receive copies of communication about the case.
--
-- /Note:/ Consider using 'ccEmailAddresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdCcEmailAddresses :: Lens.Lens' CaseDetails (Lude.Maybe [Lude.Text])
cdCcEmailAddresses = Lens.lens (ccEmailAddresses :: CaseDetails -> Lude.Maybe [Lude.Text]) (\s a -> s {ccEmailAddresses = a} :: CaseDetails)
{-# DEPRECATED cdCcEmailAddresses "Use generic-lens or generic-optics with 'ccEmailAddresses' instead." #-}

-- | The ID displayed for the case in the AWS Support Center. This is a numeric string.
--
-- /Note:/ Consider using 'displayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdDisplayId :: Lens.Lens' CaseDetails (Lude.Maybe Lude.Text)
cdDisplayId = Lens.lens (displayId :: CaseDetails -> Lude.Maybe Lude.Text) (\s a -> s {displayId = a} :: CaseDetails)
{-# DEPRECATED cdDisplayId "Use generic-lens or generic-optics with 'displayId' instead." #-}

-- | The email address of the account that submitted the case.
--
-- /Note:/ Consider using 'submittedBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdSubmittedBy :: Lens.Lens' CaseDetails (Lude.Maybe Lude.Text)
cdSubmittedBy = Lens.lens (submittedBy :: CaseDetails -> Lude.Maybe Lude.Text) (\s a -> s {submittedBy = a} :: CaseDetails)
{-# DEPRECATED cdSubmittedBy "Use generic-lens or generic-optics with 'submittedBy' instead." #-}

-- | The ISO 639-1 code for the language in which AWS provides support. AWS Support currently supports English ("en") and Japanese ("ja"). Language parameters must be passed explicitly for operations that take them.
--
-- /Note:/ Consider using 'language' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdLanguage :: Lens.Lens' CaseDetails (Lude.Maybe Lude.Text)
cdLanguage = Lens.lens (language :: CaseDetails -> Lude.Maybe Lude.Text) (\s a -> s {language = a} :: CaseDetails)
{-# DEPRECATED cdLanguage "Use generic-lens or generic-optics with 'language' instead." #-}

-- | The time that the case was created in the AWS Support Center.
--
-- /Note:/ Consider using 'timeCreated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdTimeCreated :: Lens.Lens' CaseDetails (Lude.Maybe Lude.Text)
cdTimeCreated = Lens.lens (timeCreated :: CaseDetails -> Lude.Maybe Lude.Text) (\s a -> s {timeCreated = a} :: CaseDetails)
{-# DEPRECATED cdTimeCreated "Use generic-lens or generic-optics with 'timeCreated' instead." #-}

-- | The category of problem for the AWS Support case.
--
-- /Note:/ Consider using 'categoryCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdCategoryCode :: Lens.Lens' CaseDetails (Lude.Maybe Lude.Text)
cdCategoryCode = Lens.lens (categoryCode :: CaseDetails -> Lude.Maybe Lude.Text) (\s a -> s {categoryCode = a} :: CaseDetails)
{-# DEPRECATED cdCategoryCode "Use generic-lens or generic-optics with 'categoryCode' instead." #-}

-- | The code for the AWS service. You can get a list of codes and the corresponding service names by calling 'DescribeServices' .
--
-- /Note:/ Consider using 'serviceCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdServiceCode :: Lens.Lens' CaseDetails (Lude.Maybe Lude.Text)
cdServiceCode = Lens.lens (serviceCode :: CaseDetails -> Lude.Maybe Lude.Text) (\s a -> s {serviceCode = a} :: CaseDetails)
{-# DEPRECATED cdServiceCode "Use generic-lens or generic-optics with 'serviceCode' instead." #-}

instance Lude.FromJSON CaseDetails where
  parseJSON =
    Lude.withObject
      "CaseDetails"
      ( \x ->
          CaseDetails'
            Lude.<$> (x Lude..:? "subject")
            Lude.<*> (x Lude..:? "status")
            Lude.<*> (x Lude..:? "recentCommunications")
            Lude.<*> (x Lude..:? "severityCode")
            Lude.<*> (x Lude..:? "caseId")
            Lude.<*> (x Lude..:? "ccEmailAddresses" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "displayId")
            Lude.<*> (x Lude..:? "submittedBy")
            Lude.<*> (x Lude..:? "language")
            Lude.<*> (x Lude..:? "timeCreated")
            Lude.<*> (x Lude..:? "categoryCode")
            Lude.<*> (x Lude..:? "serviceCode")
      )

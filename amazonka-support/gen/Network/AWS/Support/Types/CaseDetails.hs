{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Support.Types.CaseDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Support.Types.CaseDetails where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Support.Types.RecentCaseCommunications

-- | A JSON-formatted object that contains the metadata for a support case.
-- It is contained in the response from a DescribeCases request.
-- __CaseDetails__ contains the following fields:
--
-- -   __caseId.__ The AWS Support case ID requested or returned in the
--     call. The case ID is an alphanumeric string formatted as shown in
--     this example: case-/12345678910-2013-c4c1d2bf33c5cf47/.
--
-- -   __categoryCode.__ The category of problem for the AWS Support case.
--     Corresponds to the CategoryCode values returned by a call to
--     DescribeServices.
--
-- -   __displayId.__ The identifier for the case on pages in the AWS
--     Support Center.
--
-- -   __language.__ The ISO 639-1 code for the language in which AWS
--     provides support. AWS Support currently supports English (\"en\")
--     and Japanese (\"ja\"). Language parameters must be passed explicitly
--     for operations that take them.
--
-- -   __nextToken.__ A resumption point for pagination.
--
-- -   __recentCommunications.__ One or more Communication objects. Fields
--     of these objects are @attachments@, @body@, @caseId@, @submittedBy@,
--     and @timeCreated@.
--
-- -   __serviceCode.__ The identifier for the AWS service that corresponds
--     to the service code defined in the call to DescribeServices.
--
-- -   __severityCode.__ The severity code assigned to the case. Contains
--     one of the values returned by the call to DescribeSeverityLevels.
--     The possible values are: @low@, @normal@, @high@, @urgent@, and
--     @critical@.
--
-- -   __status.__ The status of the case in the AWS Support Center. Valid
--     values:
--
--     -   @opened@
--
--     -   @pending-customer-action@
--
--     -   @reopened@
--
--     -   @resolved@
--
--     -   @unassigned@
--
--     -   @work-in-progress@
--
-- -   __subject.__ The subject line of the case.
--
-- -   __submittedBy.__ The email address of the account that submitted the
--     case.
--
-- -   __timeCreated.__ The time the case was created, in ISO-8601 format.
--
-- /See:/ 'newCaseDetails' smart constructor.
data CaseDetails = CaseDetails'
  { -- | The ID displayed for the case in the AWS Support Center. This is a
    -- numeric string.
    displayId :: Prelude.Maybe Prelude.Text,
    -- | The status of the case.
    --
    -- Valid values:
    --
    -- -   @opened@
    --
    -- -   @pending-customer-action@
    --
    -- -   @reopened@
    --
    -- -   @resolved@
    --
    -- -   @unassigned@
    --
    -- -   @work-in-progress@
    status :: Prelude.Maybe Prelude.Text,
    -- | The AWS Support case ID requested or returned in the call. The case ID
    -- is an alphanumeric string formatted as shown in this example:
    -- case-/12345678910-2013-c4c1d2bf33c5cf47/
    caseId :: Prelude.Maybe Prelude.Text,
    -- | The five most recent communications between you and AWS Support Center,
    -- including the IDs of any attachments to the communications. Also
    -- includes a @nextToken@ that you can use to retrieve earlier
    -- communications.
    recentCommunications :: Prelude.Maybe RecentCaseCommunications,
    -- | The code for the AWS service. You can get a list of codes and the
    -- corresponding service names by calling DescribeServices.
    serviceCode :: Prelude.Maybe Prelude.Text,
    -- | The category of problem for the AWS Support case.
    categoryCode :: Prelude.Maybe Prelude.Text,
    -- | The email address of the account that submitted the case.
    submittedBy :: Prelude.Maybe Prelude.Text,
    -- | The subject line for the case in the AWS Support Center.
    subject :: Prelude.Maybe Prelude.Text,
    -- | The email addresses that receive copies of communication about the case.
    ccEmailAddresses :: Prelude.Maybe [Prelude.Text],
    -- | The code for the severity level returned by the call to
    -- DescribeSeverityLevels.
    severityCode :: Prelude.Maybe Prelude.Text,
    -- | The time that the case was created in the AWS Support Center.
    timeCreated :: Prelude.Maybe Prelude.Text,
    -- | The ISO 639-1 code for the language in which AWS provides support. AWS
    -- Support currently supports English (\"en\") and Japanese (\"ja\").
    -- Language parameters must be passed explicitly for operations that take
    -- them.
    language :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CaseDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'displayId', 'caseDetails_displayId' - The ID displayed for the case in the AWS Support Center. This is a
-- numeric string.
--
-- 'status', 'caseDetails_status' - The status of the case.
--
-- Valid values:
--
-- -   @opened@
--
-- -   @pending-customer-action@
--
-- -   @reopened@
--
-- -   @resolved@
--
-- -   @unassigned@
--
-- -   @work-in-progress@
--
-- 'caseId', 'caseDetails_caseId' - The AWS Support case ID requested or returned in the call. The case ID
-- is an alphanumeric string formatted as shown in this example:
-- case-/12345678910-2013-c4c1d2bf33c5cf47/
--
-- 'recentCommunications', 'caseDetails_recentCommunications' - The five most recent communications between you and AWS Support Center,
-- including the IDs of any attachments to the communications. Also
-- includes a @nextToken@ that you can use to retrieve earlier
-- communications.
--
-- 'serviceCode', 'caseDetails_serviceCode' - The code for the AWS service. You can get a list of codes and the
-- corresponding service names by calling DescribeServices.
--
-- 'categoryCode', 'caseDetails_categoryCode' - The category of problem for the AWS Support case.
--
-- 'submittedBy', 'caseDetails_submittedBy' - The email address of the account that submitted the case.
--
-- 'subject', 'caseDetails_subject' - The subject line for the case in the AWS Support Center.
--
-- 'ccEmailAddresses', 'caseDetails_ccEmailAddresses' - The email addresses that receive copies of communication about the case.
--
-- 'severityCode', 'caseDetails_severityCode' - The code for the severity level returned by the call to
-- DescribeSeverityLevels.
--
-- 'timeCreated', 'caseDetails_timeCreated' - The time that the case was created in the AWS Support Center.
--
-- 'language', 'caseDetails_language' - The ISO 639-1 code for the language in which AWS provides support. AWS
-- Support currently supports English (\"en\") and Japanese (\"ja\").
-- Language parameters must be passed explicitly for operations that take
-- them.
newCaseDetails ::
  CaseDetails
newCaseDetails =
  CaseDetails'
    { displayId = Prelude.Nothing,
      status = Prelude.Nothing,
      caseId = Prelude.Nothing,
      recentCommunications = Prelude.Nothing,
      serviceCode = Prelude.Nothing,
      categoryCode = Prelude.Nothing,
      submittedBy = Prelude.Nothing,
      subject = Prelude.Nothing,
      ccEmailAddresses = Prelude.Nothing,
      severityCode = Prelude.Nothing,
      timeCreated = Prelude.Nothing,
      language = Prelude.Nothing
    }

-- | The ID displayed for the case in the AWS Support Center. This is a
-- numeric string.
caseDetails_displayId :: Lens.Lens' CaseDetails (Prelude.Maybe Prelude.Text)
caseDetails_displayId = Lens.lens (\CaseDetails' {displayId} -> displayId) (\s@CaseDetails' {} a -> s {displayId = a} :: CaseDetails)

-- | The status of the case.
--
-- Valid values:
--
-- -   @opened@
--
-- -   @pending-customer-action@
--
-- -   @reopened@
--
-- -   @resolved@
--
-- -   @unassigned@
--
-- -   @work-in-progress@
caseDetails_status :: Lens.Lens' CaseDetails (Prelude.Maybe Prelude.Text)
caseDetails_status = Lens.lens (\CaseDetails' {status} -> status) (\s@CaseDetails' {} a -> s {status = a} :: CaseDetails)

-- | The AWS Support case ID requested or returned in the call. The case ID
-- is an alphanumeric string formatted as shown in this example:
-- case-/12345678910-2013-c4c1d2bf33c5cf47/
caseDetails_caseId :: Lens.Lens' CaseDetails (Prelude.Maybe Prelude.Text)
caseDetails_caseId = Lens.lens (\CaseDetails' {caseId} -> caseId) (\s@CaseDetails' {} a -> s {caseId = a} :: CaseDetails)

-- | The five most recent communications between you and AWS Support Center,
-- including the IDs of any attachments to the communications. Also
-- includes a @nextToken@ that you can use to retrieve earlier
-- communications.
caseDetails_recentCommunications :: Lens.Lens' CaseDetails (Prelude.Maybe RecentCaseCommunications)
caseDetails_recentCommunications = Lens.lens (\CaseDetails' {recentCommunications} -> recentCommunications) (\s@CaseDetails' {} a -> s {recentCommunications = a} :: CaseDetails)

-- | The code for the AWS service. You can get a list of codes and the
-- corresponding service names by calling DescribeServices.
caseDetails_serviceCode :: Lens.Lens' CaseDetails (Prelude.Maybe Prelude.Text)
caseDetails_serviceCode = Lens.lens (\CaseDetails' {serviceCode} -> serviceCode) (\s@CaseDetails' {} a -> s {serviceCode = a} :: CaseDetails)

-- | The category of problem for the AWS Support case.
caseDetails_categoryCode :: Lens.Lens' CaseDetails (Prelude.Maybe Prelude.Text)
caseDetails_categoryCode = Lens.lens (\CaseDetails' {categoryCode} -> categoryCode) (\s@CaseDetails' {} a -> s {categoryCode = a} :: CaseDetails)

-- | The email address of the account that submitted the case.
caseDetails_submittedBy :: Lens.Lens' CaseDetails (Prelude.Maybe Prelude.Text)
caseDetails_submittedBy = Lens.lens (\CaseDetails' {submittedBy} -> submittedBy) (\s@CaseDetails' {} a -> s {submittedBy = a} :: CaseDetails)

-- | The subject line for the case in the AWS Support Center.
caseDetails_subject :: Lens.Lens' CaseDetails (Prelude.Maybe Prelude.Text)
caseDetails_subject = Lens.lens (\CaseDetails' {subject} -> subject) (\s@CaseDetails' {} a -> s {subject = a} :: CaseDetails)

-- | The email addresses that receive copies of communication about the case.
caseDetails_ccEmailAddresses :: Lens.Lens' CaseDetails (Prelude.Maybe [Prelude.Text])
caseDetails_ccEmailAddresses = Lens.lens (\CaseDetails' {ccEmailAddresses} -> ccEmailAddresses) (\s@CaseDetails' {} a -> s {ccEmailAddresses = a} :: CaseDetails) Prelude.. Lens.mapping Prelude._Coerce

-- | The code for the severity level returned by the call to
-- DescribeSeverityLevels.
caseDetails_severityCode :: Lens.Lens' CaseDetails (Prelude.Maybe Prelude.Text)
caseDetails_severityCode = Lens.lens (\CaseDetails' {severityCode} -> severityCode) (\s@CaseDetails' {} a -> s {severityCode = a} :: CaseDetails)

-- | The time that the case was created in the AWS Support Center.
caseDetails_timeCreated :: Lens.Lens' CaseDetails (Prelude.Maybe Prelude.Text)
caseDetails_timeCreated = Lens.lens (\CaseDetails' {timeCreated} -> timeCreated) (\s@CaseDetails' {} a -> s {timeCreated = a} :: CaseDetails)

-- | The ISO 639-1 code for the language in which AWS provides support. AWS
-- Support currently supports English (\"en\") and Japanese (\"ja\").
-- Language parameters must be passed explicitly for operations that take
-- them.
caseDetails_language :: Lens.Lens' CaseDetails (Prelude.Maybe Prelude.Text)
caseDetails_language = Lens.lens (\CaseDetails' {language} -> language) (\s@CaseDetails' {} a -> s {language = a} :: CaseDetails)

instance Prelude.FromJSON CaseDetails where
  parseJSON =
    Prelude.withObject
      "CaseDetails"
      ( \x ->
          CaseDetails'
            Prelude.<$> (x Prelude..:? "displayId")
            Prelude.<*> (x Prelude..:? "status")
            Prelude.<*> (x Prelude..:? "caseId")
            Prelude.<*> (x Prelude..:? "recentCommunications")
            Prelude.<*> (x Prelude..:? "serviceCode")
            Prelude.<*> (x Prelude..:? "categoryCode")
            Prelude.<*> (x Prelude..:? "submittedBy")
            Prelude.<*> (x Prelude..:? "subject")
            Prelude.<*> ( x Prelude..:? "ccEmailAddresses"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "severityCode")
            Prelude.<*> (x Prelude..:? "timeCreated")
            Prelude.<*> (x Prelude..:? "language")
      )

instance Prelude.Hashable CaseDetails

instance Prelude.NFData CaseDetails

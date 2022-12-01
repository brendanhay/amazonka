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
-- Module      : Amazonka.Support.Types.CaseDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Support.Types.CaseDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Support.Types.RecentCaseCommunications

-- | A JSON-formatted object that contains the metadata for a support case.
-- It is contained in the response from a DescribeCases request.
-- __CaseDetails__ contains the following fields:
--
-- -   __caseId__ - The support case ID requested or returned in the call.
--     The case ID is an alphanumeric string formatted as shown in this
--     example: case-/12345678910-2013-c4c1d2bf33c5cf47/.
--
-- -   __categoryCode__ - The category of problem for the support case.
--     Corresponds to the @CategoryCode@ values returned by a call to
--     DescribeServices.
--
-- -   __displayId__ - The identifier for the case on pages in the Amazon
--     Web Services Support Center.
--
-- -   __language__ - The ISO 639-1 code for the language in which Amazon
--     Web Services provides support. Amazon Web Services Support currently
--     supports English (\"en\") and Japanese (\"ja\"). Language parameters
--     must be passed explicitly for operations that take them.
--
-- -   __nextToken__ - A resumption point for pagination.
--
-- -   __recentCommunications__ - One or more Communication objects. Fields
--     of these objects are @attachments@, @body@, @caseId@, @submittedBy@,
--     and @timeCreated@.
--
-- -   __serviceCode__ - The identifier for the Amazon Web Services service
--     that corresponds to the service code defined in the call to
--     DescribeServices.
--
-- -   __severityCode__ - The severity code assigned to the case. Contains
--     one of the values returned by the call to DescribeSeverityLevels.
--     The possible values are: @low@, @normal@, @high@, @urgent@, and
--     @critical@.
--
-- -   __status__ - The status of the case in the Amazon Web Services
--     Support Center. Valid values:
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
-- -   __subject__ - The subject line of the case.
--
-- -   __submittedBy__ - The email address of the account that submitted
--     the case.
--
-- -   __timeCreated__ - The time the case was created, in ISO-8601 format.
--
-- /See:/ 'newCaseDetails' smart constructor.
data CaseDetails = CaseDetails'
  { -- | The email addresses that receive copies of communication about the case.
    ccEmailAddresses :: Prelude.Maybe [Prelude.Text],
    -- | The support case ID requested or returned in the call. The case ID is an
    -- alphanumeric string formatted as shown in this example:
    -- case-/12345678910-2013-c4c1d2bf33c5cf47/
    caseId :: Prelude.Maybe Prelude.Text,
    -- | The ID displayed for the case in the Amazon Web Services Support Center.
    -- This is a numeric string.
    displayId :: Prelude.Maybe Prelude.Text,
    -- | The time that the case was created in the Amazon Web Services Support
    -- Center.
    timeCreated :: Prelude.Maybe Prelude.Text,
    -- | The category of problem for the support case.
    categoryCode :: Prelude.Maybe Prelude.Text,
    -- | The code for the Amazon Web Services service. You can get a list of
    -- codes and the corresponding service names by calling DescribeServices.
    serviceCode :: Prelude.Maybe Prelude.Text,
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
    -- | The email address of the account that submitted the case.
    submittedBy :: Prelude.Maybe Prelude.Text,
    -- | The five most recent communications between you and Amazon Web Services
    -- Support Center, including the IDs of any attachments to the
    -- communications. Also includes a @nextToken@ that you can use to retrieve
    -- earlier communications.
    recentCommunications :: Prelude.Maybe RecentCaseCommunications,
    -- | The subject line for the case in the Amazon Web Services Support Center.
    subject :: Prelude.Maybe Prelude.Text,
    -- | The ISO 639-1 code for the language in which Amazon Web Services
    -- provides support. Amazon Web Services Support currently supports English
    -- (\"en\") and Japanese (\"ja\"). Language parameters must be passed
    -- explicitly for operations that take them.
    language :: Prelude.Maybe Prelude.Text,
    -- | The code for the severity level returned by the call to
    -- DescribeSeverityLevels.
    severityCode :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CaseDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ccEmailAddresses', 'caseDetails_ccEmailAddresses' - The email addresses that receive copies of communication about the case.
--
-- 'caseId', 'caseDetails_caseId' - The support case ID requested or returned in the call. The case ID is an
-- alphanumeric string formatted as shown in this example:
-- case-/12345678910-2013-c4c1d2bf33c5cf47/
--
-- 'displayId', 'caseDetails_displayId' - The ID displayed for the case in the Amazon Web Services Support Center.
-- This is a numeric string.
--
-- 'timeCreated', 'caseDetails_timeCreated' - The time that the case was created in the Amazon Web Services Support
-- Center.
--
-- 'categoryCode', 'caseDetails_categoryCode' - The category of problem for the support case.
--
-- 'serviceCode', 'caseDetails_serviceCode' - The code for the Amazon Web Services service. You can get a list of
-- codes and the corresponding service names by calling DescribeServices.
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
-- 'submittedBy', 'caseDetails_submittedBy' - The email address of the account that submitted the case.
--
-- 'recentCommunications', 'caseDetails_recentCommunications' - The five most recent communications between you and Amazon Web Services
-- Support Center, including the IDs of any attachments to the
-- communications. Also includes a @nextToken@ that you can use to retrieve
-- earlier communications.
--
-- 'subject', 'caseDetails_subject' - The subject line for the case in the Amazon Web Services Support Center.
--
-- 'language', 'caseDetails_language' - The ISO 639-1 code for the language in which Amazon Web Services
-- provides support. Amazon Web Services Support currently supports English
-- (\"en\") and Japanese (\"ja\"). Language parameters must be passed
-- explicitly for operations that take them.
--
-- 'severityCode', 'caseDetails_severityCode' - The code for the severity level returned by the call to
-- DescribeSeverityLevels.
newCaseDetails ::
  CaseDetails
newCaseDetails =
  CaseDetails'
    { ccEmailAddresses = Prelude.Nothing,
      caseId = Prelude.Nothing,
      displayId = Prelude.Nothing,
      timeCreated = Prelude.Nothing,
      categoryCode = Prelude.Nothing,
      serviceCode = Prelude.Nothing,
      status = Prelude.Nothing,
      submittedBy = Prelude.Nothing,
      recentCommunications = Prelude.Nothing,
      subject = Prelude.Nothing,
      language = Prelude.Nothing,
      severityCode = Prelude.Nothing
    }

-- | The email addresses that receive copies of communication about the case.
caseDetails_ccEmailAddresses :: Lens.Lens' CaseDetails (Prelude.Maybe [Prelude.Text])
caseDetails_ccEmailAddresses = Lens.lens (\CaseDetails' {ccEmailAddresses} -> ccEmailAddresses) (\s@CaseDetails' {} a -> s {ccEmailAddresses = a} :: CaseDetails) Prelude.. Lens.mapping Lens.coerced

-- | The support case ID requested or returned in the call. The case ID is an
-- alphanumeric string formatted as shown in this example:
-- case-/12345678910-2013-c4c1d2bf33c5cf47/
caseDetails_caseId :: Lens.Lens' CaseDetails (Prelude.Maybe Prelude.Text)
caseDetails_caseId = Lens.lens (\CaseDetails' {caseId} -> caseId) (\s@CaseDetails' {} a -> s {caseId = a} :: CaseDetails)

-- | The ID displayed for the case in the Amazon Web Services Support Center.
-- This is a numeric string.
caseDetails_displayId :: Lens.Lens' CaseDetails (Prelude.Maybe Prelude.Text)
caseDetails_displayId = Lens.lens (\CaseDetails' {displayId} -> displayId) (\s@CaseDetails' {} a -> s {displayId = a} :: CaseDetails)

-- | The time that the case was created in the Amazon Web Services Support
-- Center.
caseDetails_timeCreated :: Lens.Lens' CaseDetails (Prelude.Maybe Prelude.Text)
caseDetails_timeCreated = Lens.lens (\CaseDetails' {timeCreated} -> timeCreated) (\s@CaseDetails' {} a -> s {timeCreated = a} :: CaseDetails)

-- | The category of problem for the support case.
caseDetails_categoryCode :: Lens.Lens' CaseDetails (Prelude.Maybe Prelude.Text)
caseDetails_categoryCode = Lens.lens (\CaseDetails' {categoryCode} -> categoryCode) (\s@CaseDetails' {} a -> s {categoryCode = a} :: CaseDetails)

-- | The code for the Amazon Web Services service. You can get a list of
-- codes and the corresponding service names by calling DescribeServices.
caseDetails_serviceCode :: Lens.Lens' CaseDetails (Prelude.Maybe Prelude.Text)
caseDetails_serviceCode = Lens.lens (\CaseDetails' {serviceCode} -> serviceCode) (\s@CaseDetails' {} a -> s {serviceCode = a} :: CaseDetails)

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

-- | The email address of the account that submitted the case.
caseDetails_submittedBy :: Lens.Lens' CaseDetails (Prelude.Maybe Prelude.Text)
caseDetails_submittedBy = Lens.lens (\CaseDetails' {submittedBy} -> submittedBy) (\s@CaseDetails' {} a -> s {submittedBy = a} :: CaseDetails)

-- | The five most recent communications between you and Amazon Web Services
-- Support Center, including the IDs of any attachments to the
-- communications. Also includes a @nextToken@ that you can use to retrieve
-- earlier communications.
caseDetails_recentCommunications :: Lens.Lens' CaseDetails (Prelude.Maybe RecentCaseCommunications)
caseDetails_recentCommunications = Lens.lens (\CaseDetails' {recentCommunications} -> recentCommunications) (\s@CaseDetails' {} a -> s {recentCommunications = a} :: CaseDetails)

-- | The subject line for the case in the Amazon Web Services Support Center.
caseDetails_subject :: Lens.Lens' CaseDetails (Prelude.Maybe Prelude.Text)
caseDetails_subject = Lens.lens (\CaseDetails' {subject} -> subject) (\s@CaseDetails' {} a -> s {subject = a} :: CaseDetails)

-- | The ISO 639-1 code for the language in which Amazon Web Services
-- provides support. Amazon Web Services Support currently supports English
-- (\"en\") and Japanese (\"ja\"). Language parameters must be passed
-- explicitly for operations that take them.
caseDetails_language :: Lens.Lens' CaseDetails (Prelude.Maybe Prelude.Text)
caseDetails_language = Lens.lens (\CaseDetails' {language} -> language) (\s@CaseDetails' {} a -> s {language = a} :: CaseDetails)

-- | The code for the severity level returned by the call to
-- DescribeSeverityLevels.
caseDetails_severityCode :: Lens.Lens' CaseDetails (Prelude.Maybe Prelude.Text)
caseDetails_severityCode = Lens.lens (\CaseDetails' {severityCode} -> severityCode) (\s@CaseDetails' {} a -> s {severityCode = a} :: CaseDetails)

instance Core.FromJSON CaseDetails where
  parseJSON =
    Core.withObject
      "CaseDetails"
      ( \x ->
          CaseDetails'
            Prelude.<$> ( x Core..:? "ccEmailAddresses"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "caseId")
            Prelude.<*> (x Core..:? "displayId")
            Prelude.<*> (x Core..:? "timeCreated")
            Prelude.<*> (x Core..:? "categoryCode")
            Prelude.<*> (x Core..:? "serviceCode")
            Prelude.<*> (x Core..:? "status")
            Prelude.<*> (x Core..:? "submittedBy")
            Prelude.<*> (x Core..:? "recentCommunications")
            Prelude.<*> (x Core..:? "subject")
            Prelude.<*> (x Core..:? "language")
            Prelude.<*> (x Core..:? "severityCode")
      )

instance Prelude.Hashable CaseDetails where
  hashWithSalt _salt CaseDetails' {..} =
    _salt `Prelude.hashWithSalt` ccEmailAddresses
      `Prelude.hashWithSalt` caseId
      `Prelude.hashWithSalt` displayId
      `Prelude.hashWithSalt` timeCreated
      `Prelude.hashWithSalt` categoryCode
      `Prelude.hashWithSalt` serviceCode
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` submittedBy
      `Prelude.hashWithSalt` recentCommunications
      `Prelude.hashWithSalt` subject
      `Prelude.hashWithSalt` language
      `Prelude.hashWithSalt` severityCode

instance Prelude.NFData CaseDetails where
  rnf CaseDetails' {..} =
    Prelude.rnf ccEmailAddresses
      `Prelude.seq` Prelude.rnf caseId
      `Prelude.seq` Prelude.rnf displayId
      `Prelude.seq` Prelude.rnf timeCreated
      `Prelude.seq` Prelude.rnf categoryCode
      `Prelude.seq` Prelude.rnf serviceCode
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf submittedBy
      `Prelude.seq` Prelude.rnf recentCommunications
      `Prelude.seq` Prelude.rnf subject
      `Prelude.seq` Prelude.rnf language
      `Prelude.seq` Prelude.rnf severityCode

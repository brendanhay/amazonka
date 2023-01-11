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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Support.Types.CaseDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
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
-- -   __language__ - The language in which Amazon Web Services Support
--     handles the case. Amazon Web Services Support currently supports
--     English (\"en\") and Japanese (\"ja\"). You must specify the ISO
--     639-1 code for the @language@ parameter if you want support in that
--     language.
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
  { -- | The support case ID requested or returned in the call. The case ID is an
    -- alphanumeric string formatted as shown in this example:
    -- case-/12345678910-2013-c4c1d2bf33c5cf47/
    caseId :: Prelude.Maybe Prelude.Text,
    -- | The category of problem for the support case.
    categoryCode :: Prelude.Maybe Prelude.Text,
    -- | The email addresses that receive copies of communication about the case.
    ccEmailAddresses :: Prelude.Maybe [Prelude.Text],
    -- | The ID displayed for the case in the Amazon Web Services Support Center.
    -- This is a numeric string.
    displayId :: Prelude.Maybe Prelude.Text,
    -- | The language in which Amazon Web Services Support handles the case.
    -- Amazon Web Services Support currently supports English (\"en\") and
    -- Japanese (\"ja\"). You must specify the ISO 639-1 code for the
    -- @language@ parameter if you want support in that language.
    language :: Prelude.Maybe Prelude.Text,
    -- | The five most recent communications between you and Amazon Web Services
    -- Support Center, including the IDs of any attachments to the
    -- communications. Also includes a @nextToken@ that you can use to retrieve
    -- earlier communications.
    recentCommunications :: Prelude.Maybe RecentCaseCommunications,
    -- | The code for the Amazon Web Services service. You can get a list of
    -- codes and the corresponding service names by calling DescribeServices.
    serviceCode :: Prelude.Maybe Prelude.Text,
    -- | The code for the severity level returned by the call to
    -- DescribeSeverityLevels.
    severityCode :: Prelude.Maybe Prelude.Text,
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
    -- | The subject line for the case in the Amazon Web Services Support Center.
    subject :: Prelude.Maybe Prelude.Text,
    -- | The email address of the account that submitted the case.
    submittedBy :: Prelude.Maybe Prelude.Text,
    -- | The time that the case was created in the Amazon Web Services Support
    -- Center.
    timeCreated :: Prelude.Maybe Prelude.Text
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
-- 'caseId', 'caseDetails_caseId' - The support case ID requested or returned in the call. The case ID is an
-- alphanumeric string formatted as shown in this example:
-- case-/12345678910-2013-c4c1d2bf33c5cf47/
--
-- 'categoryCode', 'caseDetails_categoryCode' - The category of problem for the support case.
--
-- 'ccEmailAddresses', 'caseDetails_ccEmailAddresses' - The email addresses that receive copies of communication about the case.
--
-- 'displayId', 'caseDetails_displayId' - The ID displayed for the case in the Amazon Web Services Support Center.
-- This is a numeric string.
--
-- 'language', 'caseDetails_language' - The language in which Amazon Web Services Support handles the case.
-- Amazon Web Services Support currently supports English (\"en\") and
-- Japanese (\"ja\"). You must specify the ISO 639-1 code for the
-- @language@ parameter if you want support in that language.
--
-- 'recentCommunications', 'caseDetails_recentCommunications' - The five most recent communications between you and Amazon Web Services
-- Support Center, including the IDs of any attachments to the
-- communications. Also includes a @nextToken@ that you can use to retrieve
-- earlier communications.
--
-- 'serviceCode', 'caseDetails_serviceCode' - The code for the Amazon Web Services service. You can get a list of
-- codes and the corresponding service names by calling DescribeServices.
--
-- 'severityCode', 'caseDetails_severityCode' - The code for the severity level returned by the call to
-- DescribeSeverityLevels.
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
-- 'subject', 'caseDetails_subject' - The subject line for the case in the Amazon Web Services Support Center.
--
-- 'submittedBy', 'caseDetails_submittedBy' - The email address of the account that submitted the case.
--
-- 'timeCreated', 'caseDetails_timeCreated' - The time that the case was created in the Amazon Web Services Support
-- Center.
newCaseDetails ::
  CaseDetails
newCaseDetails =
  CaseDetails'
    { caseId = Prelude.Nothing,
      categoryCode = Prelude.Nothing,
      ccEmailAddresses = Prelude.Nothing,
      displayId = Prelude.Nothing,
      language = Prelude.Nothing,
      recentCommunications = Prelude.Nothing,
      serviceCode = Prelude.Nothing,
      severityCode = Prelude.Nothing,
      status = Prelude.Nothing,
      subject = Prelude.Nothing,
      submittedBy = Prelude.Nothing,
      timeCreated = Prelude.Nothing
    }

-- | The support case ID requested or returned in the call. The case ID is an
-- alphanumeric string formatted as shown in this example:
-- case-/12345678910-2013-c4c1d2bf33c5cf47/
caseDetails_caseId :: Lens.Lens' CaseDetails (Prelude.Maybe Prelude.Text)
caseDetails_caseId = Lens.lens (\CaseDetails' {caseId} -> caseId) (\s@CaseDetails' {} a -> s {caseId = a} :: CaseDetails)

-- | The category of problem for the support case.
caseDetails_categoryCode :: Lens.Lens' CaseDetails (Prelude.Maybe Prelude.Text)
caseDetails_categoryCode = Lens.lens (\CaseDetails' {categoryCode} -> categoryCode) (\s@CaseDetails' {} a -> s {categoryCode = a} :: CaseDetails)

-- | The email addresses that receive copies of communication about the case.
caseDetails_ccEmailAddresses :: Lens.Lens' CaseDetails (Prelude.Maybe [Prelude.Text])
caseDetails_ccEmailAddresses = Lens.lens (\CaseDetails' {ccEmailAddresses} -> ccEmailAddresses) (\s@CaseDetails' {} a -> s {ccEmailAddresses = a} :: CaseDetails) Prelude.. Lens.mapping Lens.coerced

-- | The ID displayed for the case in the Amazon Web Services Support Center.
-- This is a numeric string.
caseDetails_displayId :: Lens.Lens' CaseDetails (Prelude.Maybe Prelude.Text)
caseDetails_displayId = Lens.lens (\CaseDetails' {displayId} -> displayId) (\s@CaseDetails' {} a -> s {displayId = a} :: CaseDetails)

-- | The language in which Amazon Web Services Support handles the case.
-- Amazon Web Services Support currently supports English (\"en\") and
-- Japanese (\"ja\"). You must specify the ISO 639-1 code for the
-- @language@ parameter if you want support in that language.
caseDetails_language :: Lens.Lens' CaseDetails (Prelude.Maybe Prelude.Text)
caseDetails_language = Lens.lens (\CaseDetails' {language} -> language) (\s@CaseDetails' {} a -> s {language = a} :: CaseDetails)

-- | The five most recent communications between you and Amazon Web Services
-- Support Center, including the IDs of any attachments to the
-- communications. Also includes a @nextToken@ that you can use to retrieve
-- earlier communications.
caseDetails_recentCommunications :: Lens.Lens' CaseDetails (Prelude.Maybe RecentCaseCommunications)
caseDetails_recentCommunications = Lens.lens (\CaseDetails' {recentCommunications} -> recentCommunications) (\s@CaseDetails' {} a -> s {recentCommunications = a} :: CaseDetails)

-- | The code for the Amazon Web Services service. You can get a list of
-- codes and the corresponding service names by calling DescribeServices.
caseDetails_serviceCode :: Lens.Lens' CaseDetails (Prelude.Maybe Prelude.Text)
caseDetails_serviceCode = Lens.lens (\CaseDetails' {serviceCode} -> serviceCode) (\s@CaseDetails' {} a -> s {serviceCode = a} :: CaseDetails)

-- | The code for the severity level returned by the call to
-- DescribeSeverityLevels.
caseDetails_severityCode :: Lens.Lens' CaseDetails (Prelude.Maybe Prelude.Text)
caseDetails_severityCode = Lens.lens (\CaseDetails' {severityCode} -> severityCode) (\s@CaseDetails' {} a -> s {severityCode = a} :: CaseDetails)

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

-- | The subject line for the case in the Amazon Web Services Support Center.
caseDetails_subject :: Lens.Lens' CaseDetails (Prelude.Maybe Prelude.Text)
caseDetails_subject = Lens.lens (\CaseDetails' {subject} -> subject) (\s@CaseDetails' {} a -> s {subject = a} :: CaseDetails)

-- | The email address of the account that submitted the case.
caseDetails_submittedBy :: Lens.Lens' CaseDetails (Prelude.Maybe Prelude.Text)
caseDetails_submittedBy = Lens.lens (\CaseDetails' {submittedBy} -> submittedBy) (\s@CaseDetails' {} a -> s {submittedBy = a} :: CaseDetails)

-- | The time that the case was created in the Amazon Web Services Support
-- Center.
caseDetails_timeCreated :: Lens.Lens' CaseDetails (Prelude.Maybe Prelude.Text)
caseDetails_timeCreated = Lens.lens (\CaseDetails' {timeCreated} -> timeCreated) (\s@CaseDetails' {} a -> s {timeCreated = a} :: CaseDetails)

instance Data.FromJSON CaseDetails where
  parseJSON =
    Data.withObject
      "CaseDetails"
      ( \x ->
          CaseDetails'
            Prelude.<$> (x Data..:? "caseId")
            Prelude.<*> (x Data..:? "categoryCode")
            Prelude.<*> ( x Data..:? "ccEmailAddresses"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "displayId")
            Prelude.<*> (x Data..:? "language")
            Prelude.<*> (x Data..:? "recentCommunications")
            Prelude.<*> (x Data..:? "serviceCode")
            Prelude.<*> (x Data..:? "severityCode")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "subject")
            Prelude.<*> (x Data..:? "submittedBy")
            Prelude.<*> (x Data..:? "timeCreated")
      )

instance Prelude.Hashable CaseDetails where
  hashWithSalt _salt CaseDetails' {..} =
    _salt `Prelude.hashWithSalt` caseId
      `Prelude.hashWithSalt` categoryCode
      `Prelude.hashWithSalt` ccEmailAddresses
      `Prelude.hashWithSalt` displayId
      `Prelude.hashWithSalt` language
      `Prelude.hashWithSalt` recentCommunications
      `Prelude.hashWithSalt` serviceCode
      `Prelude.hashWithSalt` severityCode
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` subject
      `Prelude.hashWithSalt` submittedBy
      `Prelude.hashWithSalt` timeCreated

instance Prelude.NFData CaseDetails where
  rnf CaseDetails' {..} =
    Prelude.rnf caseId
      `Prelude.seq` Prelude.rnf categoryCode
      `Prelude.seq` Prelude.rnf ccEmailAddresses
      `Prelude.seq` Prelude.rnf displayId
      `Prelude.seq` Prelude.rnf language
      `Prelude.seq` Prelude.rnf recentCommunications
      `Prelude.seq` Prelude.rnf serviceCode
      `Prelude.seq` Prelude.rnf severityCode
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf subject
      `Prelude.seq` Prelude.rnf submittedBy
      `Prelude.seq` Prelude.rnf timeCreated

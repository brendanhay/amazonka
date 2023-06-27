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
-- Module      : Amazonka.AuditManager.Types.Evidence
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AuditManager.Types.Evidence where

import Amazonka.AuditManager.Types.Resource
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A record that contains the information needed to demonstrate compliance
-- with the requirements specified by a control. Examples of evidence
-- include change activity invoked by a user, or a system configuration
-- snapshot.
--
-- /See:/ 'newEvidence' smart constructor.
data Evidence = Evidence'
  { -- | Specifies whether the evidence is included in the assessment report.
    assessmentReportSelection :: Prelude.Maybe Prelude.Text,
    -- | The names and values that are used by the evidence event. This includes
    -- an attribute name (such as @allowUsersToChangePassword@) and value (such
    -- as @true@ or @false@).
    attributes :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The identifier for the Amazon Web Services account.
    awsAccountId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services account that the evidence is collected from, and
    -- its organization path.
    awsOrganization :: Prelude.Maybe Prelude.Text,
    -- | The evaluation status for automated evidence that falls under the
    -- compliance check category.
    --
    -- -   Audit Manager classes evidence as non-compliant if Security Hub
    --     reports a /Fail/ result, or if Config reports a /Non-compliant/
    --     result.
    --
    -- -   Audit Manager classes evidence as compliant if Security Hub reports
    --     a /Pass/ result, or if Config reports a /Compliant/ result.
    --
    -- -   If a compliance check isn\'t available or applicable, then no
    --     compliance evaluation can be made for that evidence. This is the
    --     case if the evidence uses Config or Security Hub as the underlying
    --     data source type, but those services aren\'t enabled. This is also
    --     the case if the evidence uses an underlying data source type that
    --     doesn\'t support compliance checks (such as manual evidence, Amazon
    --     Web Services API calls, or CloudTrail).
    complianceCheck :: Prelude.Maybe Prelude.Text,
    -- | The data source where the evidence was collected from.
    dataSource :: Prelude.Maybe Prelude.Text,
    -- | The name of the evidence event.
    eventName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Service that the evidence is collected from.
    eventSource :: Prelude.Maybe Prelude.Text,
    -- | The identifier for the Amazon Web Services account.
    evidenceAwsAccountId :: Prelude.Maybe Prelude.Text,
    -- | The type of automated evidence.
    evidenceByType :: Prelude.Maybe Prelude.Text,
    -- | The identifier for the folder that the evidence is stored in.
    evidenceFolderId :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the user or role that\'s associated with the
    -- evidence.
    iamId :: Prelude.Maybe Prelude.Text,
    -- | The identifier for the evidence.
    id :: Prelude.Maybe Prelude.Text,
    -- | The list of resources that are assessed to generate the evidence.
    resourcesIncluded :: Prelude.Maybe [Resource],
    -- | The timestamp that represents when the evidence was collected.
    time :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Evidence' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assessmentReportSelection', 'evidence_assessmentReportSelection' - Specifies whether the evidence is included in the assessment report.
--
-- 'attributes', 'evidence_attributes' - The names and values that are used by the evidence event. This includes
-- an attribute name (such as @allowUsersToChangePassword@) and value (such
-- as @true@ or @false@).
--
-- 'awsAccountId', 'evidence_awsAccountId' - The identifier for the Amazon Web Services account.
--
-- 'awsOrganization', 'evidence_awsOrganization' - The Amazon Web Services account that the evidence is collected from, and
-- its organization path.
--
-- 'complianceCheck', 'evidence_complianceCheck' - The evaluation status for automated evidence that falls under the
-- compliance check category.
--
-- -   Audit Manager classes evidence as non-compliant if Security Hub
--     reports a /Fail/ result, or if Config reports a /Non-compliant/
--     result.
--
-- -   Audit Manager classes evidence as compliant if Security Hub reports
--     a /Pass/ result, or if Config reports a /Compliant/ result.
--
-- -   If a compliance check isn\'t available or applicable, then no
--     compliance evaluation can be made for that evidence. This is the
--     case if the evidence uses Config or Security Hub as the underlying
--     data source type, but those services aren\'t enabled. This is also
--     the case if the evidence uses an underlying data source type that
--     doesn\'t support compliance checks (such as manual evidence, Amazon
--     Web Services API calls, or CloudTrail).
--
-- 'dataSource', 'evidence_dataSource' - The data source where the evidence was collected from.
--
-- 'eventName', 'evidence_eventName' - The name of the evidence event.
--
-- 'eventSource', 'evidence_eventSource' - The Amazon Web Service that the evidence is collected from.
--
-- 'evidenceAwsAccountId', 'evidence_evidenceAwsAccountId' - The identifier for the Amazon Web Services account.
--
-- 'evidenceByType', 'evidence_evidenceByType' - The type of automated evidence.
--
-- 'evidenceFolderId', 'evidence_evidenceFolderId' - The identifier for the folder that the evidence is stored in.
--
-- 'iamId', 'evidence_iamId' - The unique identifier for the user or role that\'s associated with the
-- evidence.
--
-- 'id', 'evidence_id' - The identifier for the evidence.
--
-- 'resourcesIncluded', 'evidence_resourcesIncluded' - The list of resources that are assessed to generate the evidence.
--
-- 'time', 'evidence_time' - The timestamp that represents when the evidence was collected.
newEvidence ::
  Evidence
newEvidence =
  Evidence'
    { assessmentReportSelection =
        Prelude.Nothing,
      attributes = Prelude.Nothing,
      awsAccountId = Prelude.Nothing,
      awsOrganization = Prelude.Nothing,
      complianceCheck = Prelude.Nothing,
      dataSource = Prelude.Nothing,
      eventName = Prelude.Nothing,
      eventSource = Prelude.Nothing,
      evidenceAwsAccountId = Prelude.Nothing,
      evidenceByType = Prelude.Nothing,
      evidenceFolderId = Prelude.Nothing,
      iamId = Prelude.Nothing,
      id = Prelude.Nothing,
      resourcesIncluded = Prelude.Nothing,
      time = Prelude.Nothing
    }

-- | Specifies whether the evidence is included in the assessment report.
evidence_assessmentReportSelection :: Lens.Lens' Evidence (Prelude.Maybe Prelude.Text)
evidence_assessmentReportSelection = Lens.lens (\Evidence' {assessmentReportSelection} -> assessmentReportSelection) (\s@Evidence' {} a -> s {assessmentReportSelection = a} :: Evidence)

-- | The names and values that are used by the evidence event. This includes
-- an attribute name (such as @allowUsersToChangePassword@) and value (such
-- as @true@ or @false@).
evidence_attributes :: Lens.Lens' Evidence (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
evidence_attributes = Lens.lens (\Evidence' {attributes} -> attributes) (\s@Evidence' {} a -> s {attributes = a} :: Evidence) Prelude.. Lens.mapping Lens.coerced

-- | The identifier for the Amazon Web Services account.
evidence_awsAccountId :: Lens.Lens' Evidence (Prelude.Maybe Prelude.Text)
evidence_awsAccountId = Lens.lens (\Evidence' {awsAccountId} -> awsAccountId) (\s@Evidence' {} a -> s {awsAccountId = a} :: Evidence)

-- | The Amazon Web Services account that the evidence is collected from, and
-- its organization path.
evidence_awsOrganization :: Lens.Lens' Evidence (Prelude.Maybe Prelude.Text)
evidence_awsOrganization = Lens.lens (\Evidence' {awsOrganization} -> awsOrganization) (\s@Evidence' {} a -> s {awsOrganization = a} :: Evidence)

-- | The evaluation status for automated evidence that falls under the
-- compliance check category.
--
-- -   Audit Manager classes evidence as non-compliant if Security Hub
--     reports a /Fail/ result, or if Config reports a /Non-compliant/
--     result.
--
-- -   Audit Manager classes evidence as compliant if Security Hub reports
--     a /Pass/ result, or if Config reports a /Compliant/ result.
--
-- -   If a compliance check isn\'t available or applicable, then no
--     compliance evaluation can be made for that evidence. This is the
--     case if the evidence uses Config or Security Hub as the underlying
--     data source type, but those services aren\'t enabled. This is also
--     the case if the evidence uses an underlying data source type that
--     doesn\'t support compliance checks (such as manual evidence, Amazon
--     Web Services API calls, or CloudTrail).
evidence_complianceCheck :: Lens.Lens' Evidence (Prelude.Maybe Prelude.Text)
evidence_complianceCheck = Lens.lens (\Evidence' {complianceCheck} -> complianceCheck) (\s@Evidence' {} a -> s {complianceCheck = a} :: Evidence)

-- | The data source where the evidence was collected from.
evidence_dataSource :: Lens.Lens' Evidence (Prelude.Maybe Prelude.Text)
evidence_dataSource = Lens.lens (\Evidence' {dataSource} -> dataSource) (\s@Evidence' {} a -> s {dataSource = a} :: Evidence)

-- | The name of the evidence event.
evidence_eventName :: Lens.Lens' Evidence (Prelude.Maybe Prelude.Text)
evidence_eventName = Lens.lens (\Evidence' {eventName} -> eventName) (\s@Evidence' {} a -> s {eventName = a} :: Evidence)

-- | The Amazon Web Service that the evidence is collected from.
evidence_eventSource :: Lens.Lens' Evidence (Prelude.Maybe Prelude.Text)
evidence_eventSource = Lens.lens (\Evidence' {eventSource} -> eventSource) (\s@Evidence' {} a -> s {eventSource = a} :: Evidence)

-- | The identifier for the Amazon Web Services account.
evidence_evidenceAwsAccountId :: Lens.Lens' Evidence (Prelude.Maybe Prelude.Text)
evidence_evidenceAwsAccountId = Lens.lens (\Evidence' {evidenceAwsAccountId} -> evidenceAwsAccountId) (\s@Evidence' {} a -> s {evidenceAwsAccountId = a} :: Evidence)

-- | The type of automated evidence.
evidence_evidenceByType :: Lens.Lens' Evidence (Prelude.Maybe Prelude.Text)
evidence_evidenceByType = Lens.lens (\Evidence' {evidenceByType} -> evidenceByType) (\s@Evidence' {} a -> s {evidenceByType = a} :: Evidence)

-- | The identifier for the folder that the evidence is stored in.
evidence_evidenceFolderId :: Lens.Lens' Evidence (Prelude.Maybe Prelude.Text)
evidence_evidenceFolderId = Lens.lens (\Evidence' {evidenceFolderId} -> evidenceFolderId) (\s@Evidence' {} a -> s {evidenceFolderId = a} :: Evidence)

-- | The unique identifier for the user or role that\'s associated with the
-- evidence.
evidence_iamId :: Lens.Lens' Evidence (Prelude.Maybe Prelude.Text)
evidence_iamId = Lens.lens (\Evidence' {iamId} -> iamId) (\s@Evidence' {} a -> s {iamId = a} :: Evidence)

-- | The identifier for the evidence.
evidence_id :: Lens.Lens' Evidence (Prelude.Maybe Prelude.Text)
evidence_id = Lens.lens (\Evidence' {id} -> id) (\s@Evidence' {} a -> s {id = a} :: Evidence)

-- | The list of resources that are assessed to generate the evidence.
evidence_resourcesIncluded :: Lens.Lens' Evidence (Prelude.Maybe [Resource])
evidence_resourcesIncluded = Lens.lens (\Evidence' {resourcesIncluded} -> resourcesIncluded) (\s@Evidence' {} a -> s {resourcesIncluded = a} :: Evidence) Prelude.. Lens.mapping Lens.coerced

-- | The timestamp that represents when the evidence was collected.
evidence_time :: Lens.Lens' Evidence (Prelude.Maybe Prelude.UTCTime)
evidence_time = Lens.lens (\Evidence' {time} -> time) (\s@Evidence' {} a -> s {time = a} :: Evidence) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON Evidence where
  parseJSON =
    Data.withObject
      "Evidence"
      ( \x ->
          Evidence'
            Prelude.<$> (x Data..:? "assessmentReportSelection")
            Prelude.<*> (x Data..:? "attributes" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "awsAccountId")
            Prelude.<*> (x Data..:? "awsOrganization")
            Prelude.<*> (x Data..:? "complianceCheck")
            Prelude.<*> (x Data..:? "dataSource")
            Prelude.<*> (x Data..:? "eventName")
            Prelude.<*> (x Data..:? "eventSource")
            Prelude.<*> (x Data..:? "evidenceAwsAccountId")
            Prelude.<*> (x Data..:? "evidenceByType")
            Prelude.<*> (x Data..:? "evidenceFolderId")
            Prelude.<*> (x Data..:? "iamId")
            Prelude.<*> (x Data..:? "id")
            Prelude.<*> ( x
                            Data..:? "resourcesIncluded"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "time")
      )

instance Prelude.Hashable Evidence where
  hashWithSalt _salt Evidence' {..} =
    _salt
      `Prelude.hashWithSalt` assessmentReportSelection
      `Prelude.hashWithSalt` attributes
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` awsOrganization
      `Prelude.hashWithSalt` complianceCheck
      `Prelude.hashWithSalt` dataSource
      `Prelude.hashWithSalt` eventName
      `Prelude.hashWithSalt` eventSource
      `Prelude.hashWithSalt` evidenceAwsAccountId
      `Prelude.hashWithSalt` evidenceByType
      `Prelude.hashWithSalt` evidenceFolderId
      `Prelude.hashWithSalt` iamId
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` resourcesIncluded
      `Prelude.hashWithSalt` time

instance Prelude.NFData Evidence where
  rnf Evidence' {..} =
    Prelude.rnf assessmentReportSelection
      `Prelude.seq` Prelude.rnf attributes
      `Prelude.seq` Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf awsOrganization
      `Prelude.seq` Prelude.rnf complianceCheck
      `Prelude.seq` Prelude.rnf dataSource
      `Prelude.seq` Prelude.rnf eventName
      `Prelude.seq` Prelude.rnf eventSource
      `Prelude.seq` Prelude.rnf evidenceAwsAccountId
      `Prelude.seq` Prelude.rnf evidenceByType
      `Prelude.seq` Prelude.rnf evidenceFolderId
      `Prelude.seq` Prelude.rnf iamId
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf resourcesIncluded
      `Prelude.seq` Prelude.rnf time

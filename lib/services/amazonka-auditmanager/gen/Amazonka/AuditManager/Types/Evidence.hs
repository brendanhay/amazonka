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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AuditManager.Types.Evidence where

import Amazonka.AuditManager.Types.Resource
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A record that contains the information needed to demonstrate compliance
-- with the requirements specified by a control. Examples of evidence
-- include change activity triggered by a user, or a system configuration
-- snapshot.
--
-- /See:/ 'newEvidence' smart constructor.
data Evidence = Evidence'
  { -- | The identifier for the Amazon Web Services account.
    awsAccountId :: Prelude.Maybe Prelude.Text,
    -- | The identifier for the Amazon Web Services account.
    evidenceAwsAccountId :: Prelude.Maybe Prelude.Text,
    -- | The identifier for the folder that the evidence is stored in.
    evidenceFolderId :: Prelude.Maybe Prelude.Text,
    -- | The list of resources that are assessed to generate the evidence.
    resourcesIncluded :: Prelude.Maybe [Resource],
    -- | The Amazon Web Services account that the evidence is collected from, and
    -- its organization path.
    awsOrganization :: Prelude.Maybe Prelude.Text,
    -- | The timestamp that represents when the evidence was collected.
    time :: Prelude.Maybe Core.POSIX,
    -- | The identifier for the evidence.
    id :: Prelude.Maybe Prelude.Text,
    -- | The type of automated evidence.
    evidenceByType :: Prelude.Maybe Prelude.Text,
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
    -- | The unique identifier for the IAM user or role that\'s associated with
    -- the evidence.
    iamId :: Prelude.Maybe Prelude.Text,
    -- | The name of the evidence event.
    eventName :: Prelude.Maybe Prelude.Text,
    -- | The data source where the evidence was collected from.
    dataSource :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the evidence is included in the assessment report.
    assessmentReportSelection :: Prelude.Maybe Prelude.Text,
    -- | The names and values that are used by the evidence event. This includes
    -- an attribute name (such as @allowUsersToChangePassword@) and value (such
    -- as @true@ or @false@).
    attributes :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The Amazon Web Service that the evidence is collected from.
    eventSource :: Prelude.Maybe Prelude.Text
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
-- 'awsAccountId', 'evidence_awsAccountId' - The identifier for the Amazon Web Services account.
--
-- 'evidenceAwsAccountId', 'evidence_evidenceAwsAccountId' - The identifier for the Amazon Web Services account.
--
-- 'evidenceFolderId', 'evidence_evidenceFolderId' - The identifier for the folder that the evidence is stored in.
--
-- 'resourcesIncluded', 'evidence_resourcesIncluded' - The list of resources that are assessed to generate the evidence.
--
-- 'awsOrganization', 'evidence_awsOrganization' - The Amazon Web Services account that the evidence is collected from, and
-- its organization path.
--
-- 'time', 'evidence_time' - The timestamp that represents when the evidence was collected.
--
-- 'id', 'evidence_id' - The identifier for the evidence.
--
-- 'evidenceByType', 'evidence_evidenceByType' - The type of automated evidence.
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
-- 'iamId', 'evidence_iamId' - The unique identifier for the IAM user or role that\'s associated with
-- the evidence.
--
-- 'eventName', 'evidence_eventName' - The name of the evidence event.
--
-- 'dataSource', 'evidence_dataSource' - The data source where the evidence was collected from.
--
-- 'assessmentReportSelection', 'evidence_assessmentReportSelection' - Specifies whether the evidence is included in the assessment report.
--
-- 'attributes', 'evidence_attributes' - The names and values that are used by the evidence event. This includes
-- an attribute name (such as @allowUsersToChangePassword@) and value (such
-- as @true@ or @false@).
--
-- 'eventSource', 'evidence_eventSource' - The Amazon Web Service that the evidence is collected from.
newEvidence ::
  Evidence
newEvidence =
  Evidence'
    { awsAccountId = Prelude.Nothing,
      evidenceAwsAccountId = Prelude.Nothing,
      evidenceFolderId = Prelude.Nothing,
      resourcesIncluded = Prelude.Nothing,
      awsOrganization = Prelude.Nothing,
      time = Prelude.Nothing,
      id = Prelude.Nothing,
      evidenceByType = Prelude.Nothing,
      complianceCheck = Prelude.Nothing,
      iamId = Prelude.Nothing,
      eventName = Prelude.Nothing,
      dataSource = Prelude.Nothing,
      assessmentReportSelection = Prelude.Nothing,
      attributes = Prelude.Nothing,
      eventSource = Prelude.Nothing
    }

-- | The identifier for the Amazon Web Services account.
evidence_awsAccountId :: Lens.Lens' Evidence (Prelude.Maybe Prelude.Text)
evidence_awsAccountId = Lens.lens (\Evidence' {awsAccountId} -> awsAccountId) (\s@Evidence' {} a -> s {awsAccountId = a} :: Evidence)

-- | The identifier for the Amazon Web Services account.
evidence_evidenceAwsAccountId :: Lens.Lens' Evidence (Prelude.Maybe Prelude.Text)
evidence_evidenceAwsAccountId = Lens.lens (\Evidence' {evidenceAwsAccountId} -> evidenceAwsAccountId) (\s@Evidence' {} a -> s {evidenceAwsAccountId = a} :: Evidence)

-- | The identifier for the folder that the evidence is stored in.
evidence_evidenceFolderId :: Lens.Lens' Evidence (Prelude.Maybe Prelude.Text)
evidence_evidenceFolderId = Lens.lens (\Evidence' {evidenceFolderId} -> evidenceFolderId) (\s@Evidence' {} a -> s {evidenceFolderId = a} :: Evidence)

-- | The list of resources that are assessed to generate the evidence.
evidence_resourcesIncluded :: Lens.Lens' Evidence (Prelude.Maybe [Resource])
evidence_resourcesIncluded = Lens.lens (\Evidence' {resourcesIncluded} -> resourcesIncluded) (\s@Evidence' {} a -> s {resourcesIncluded = a} :: Evidence) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Web Services account that the evidence is collected from, and
-- its organization path.
evidence_awsOrganization :: Lens.Lens' Evidence (Prelude.Maybe Prelude.Text)
evidence_awsOrganization = Lens.lens (\Evidence' {awsOrganization} -> awsOrganization) (\s@Evidence' {} a -> s {awsOrganization = a} :: Evidence)

-- | The timestamp that represents when the evidence was collected.
evidence_time :: Lens.Lens' Evidence (Prelude.Maybe Prelude.UTCTime)
evidence_time = Lens.lens (\Evidence' {time} -> time) (\s@Evidence' {} a -> s {time = a} :: Evidence) Prelude.. Lens.mapping Core._Time

-- | The identifier for the evidence.
evidence_id :: Lens.Lens' Evidence (Prelude.Maybe Prelude.Text)
evidence_id = Lens.lens (\Evidence' {id} -> id) (\s@Evidence' {} a -> s {id = a} :: Evidence)

-- | The type of automated evidence.
evidence_evidenceByType :: Lens.Lens' Evidence (Prelude.Maybe Prelude.Text)
evidence_evidenceByType = Lens.lens (\Evidence' {evidenceByType} -> evidenceByType) (\s@Evidence' {} a -> s {evidenceByType = a} :: Evidence)

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

-- | The unique identifier for the IAM user or role that\'s associated with
-- the evidence.
evidence_iamId :: Lens.Lens' Evidence (Prelude.Maybe Prelude.Text)
evidence_iamId = Lens.lens (\Evidence' {iamId} -> iamId) (\s@Evidence' {} a -> s {iamId = a} :: Evidence)

-- | The name of the evidence event.
evidence_eventName :: Lens.Lens' Evidence (Prelude.Maybe Prelude.Text)
evidence_eventName = Lens.lens (\Evidence' {eventName} -> eventName) (\s@Evidence' {} a -> s {eventName = a} :: Evidence)

-- | The data source where the evidence was collected from.
evidence_dataSource :: Lens.Lens' Evidence (Prelude.Maybe Prelude.Text)
evidence_dataSource = Lens.lens (\Evidence' {dataSource} -> dataSource) (\s@Evidence' {} a -> s {dataSource = a} :: Evidence)

-- | Specifies whether the evidence is included in the assessment report.
evidence_assessmentReportSelection :: Lens.Lens' Evidence (Prelude.Maybe Prelude.Text)
evidence_assessmentReportSelection = Lens.lens (\Evidence' {assessmentReportSelection} -> assessmentReportSelection) (\s@Evidence' {} a -> s {assessmentReportSelection = a} :: Evidence)

-- | The names and values that are used by the evidence event. This includes
-- an attribute name (such as @allowUsersToChangePassword@) and value (such
-- as @true@ or @false@).
evidence_attributes :: Lens.Lens' Evidence (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
evidence_attributes = Lens.lens (\Evidence' {attributes} -> attributes) (\s@Evidence' {} a -> s {attributes = a} :: Evidence) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Web Service that the evidence is collected from.
evidence_eventSource :: Lens.Lens' Evidence (Prelude.Maybe Prelude.Text)
evidence_eventSource = Lens.lens (\Evidence' {eventSource} -> eventSource) (\s@Evidence' {} a -> s {eventSource = a} :: Evidence)

instance Core.FromJSON Evidence where
  parseJSON =
    Core.withObject
      "Evidence"
      ( \x ->
          Evidence'
            Prelude.<$> (x Core..:? "awsAccountId")
            Prelude.<*> (x Core..:? "evidenceAwsAccountId")
            Prelude.<*> (x Core..:? "evidenceFolderId")
            Prelude.<*> ( x Core..:? "resourcesIncluded"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "awsOrganization")
            Prelude.<*> (x Core..:? "time")
            Prelude.<*> (x Core..:? "id")
            Prelude.<*> (x Core..:? "evidenceByType")
            Prelude.<*> (x Core..:? "complianceCheck")
            Prelude.<*> (x Core..:? "iamId")
            Prelude.<*> (x Core..:? "eventName")
            Prelude.<*> (x Core..:? "dataSource")
            Prelude.<*> (x Core..:? "assessmentReportSelection")
            Prelude.<*> (x Core..:? "attributes" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "eventSource")
      )

instance Prelude.Hashable Evidence where
  hashWithSalt _salt Evidence' {..} =
    _salt `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` evidenceAwsAccountId
      `Prelude.hashWithSalt` evidenceFolderId
      `Prelude.hashWithSalt` resourcesIncluded
      `Prelude.hashWithSalt` awsOrganization
      `Prelude.hashWithSalt` time
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` evidenceByType
      `Prelude.hashWithSalt` complianceCheck
      `Prelude.hashWithSalt` iamId
      `Prelude.hashWithSalt` eventName
      `Prelude.hashWithSalt` dataSource
      `Prelude.hashWithSalt` assessmentReportSelection
      `Prelude.hashWithSalt` attributes
      `Prelude.hashWithSalt` eventSource

instance Prelude.NFData Evidence where
  rnf Evidence' {..} =
    Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf evidenceAwsAccountId
      `Prelude.seq` Prelude.rnf evidenceFolderId
      `Prelude.seq` Prelude.rnf resourcesIncluded
      `Prelude.seq` Prelude.rnf awsOrganization
      `Prelude.seq` Prelude.rnf time
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf evidenceByType
      `Prelude.seq` Prelude.rnf complianceCheck
      `Prelude.seq` Prelude.rnf iamId
      `Prelude.seq` Prelude.rnf eventName
      `Prelude.seq` Prelude.rnf dataSource
      `Prelude.seq` Prelude.rnf assessmentReportSelection
      `Prelude.seq` Prelude.rnf attributes
      `Prelude.seq` Prelude.rnf eventSource

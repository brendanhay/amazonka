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
-- Module      : Amazonka.AuditManager.Types.AssessmentFrameworkShareRequest
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AuditManager.Types.AssessmentFrameworkShareRequest where

import Amazonka.AuditManager.Types.ShareRequestStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Represents a share request for a custom framework in Audit Manager.
--
-- /See:/ 'newAssessmentFrameworkShareRequest' smart constructor.
data AssessmentFrameworkShareRequest = AssessmentFrameworkShareRequest'
  { -- | The number of custom controls that are part of the shared custom
    -- framework.
    customControlsCount :: Prelude.Maybe Prelude.Int,
    -- | The Amazon Web Services account of the recipient.
    destinationAccount :: Prelude.Maybe Prelude.Text,
    -- | The time when the share request expires.
    expirationTime :: Prelude.Maybe Core.POSIX,
    -- | The description of the shared custom framework.
    frameworkDescription :: Prelude.Maybe Prelude.Text,
    -- | The status of the share request.
    status :: Prelude.Maybe ShareRequestStatus,
    -- | The unique identifier for the share request.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of the custom framework that the share request is for.
    frameworkName :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the shared custom framework.
    frameworkId :: Prelude.Maybe Prelude.Text,
    -- | Specifies when the share request was last updated.
    lastUpdated :: Prelude.Maybe Core.POSIX,
    -- | An optional comment from the sender about the share request.
    comment :: Prelude.Maybe Prelude.Text,
    -- | The time when the share request was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Web Services Region of the recipient.
    destinationRegion :: Prelude.Maybe Prelude.Text,
    -- | The compliance type that the shared custom framework supports, such as
    -- CIS or HIPAA.
    complianceType :: Prelude.Maybe Prelude.Text,
    -- | The number of standard controls that are part of the shared custom
    -- framework.
    standardControlsCount :: Prelude.Maybe Prelude.Int,
    -- | The Amazon Web Services account of the sender.
    sourceAccount :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssessmentFrameworkShareRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customControlsCount', 'assessmentFrameworkShareRequest_customControlsCount' - The number of custom controls that are part of the shared custom
-- framework.
--
-- 'destinationAccount', 'assessmentFrameworkShareRequest_destinationAccount' - The Amazon Web Services account of the recipient.
--
-- 'expirationTime', 'assessmentFrameworkShareRequest_expirationTime' - The time when the share request expires.
--
-- 'frameworkDescription', 'assessmentFrameworkShareRequest_frameworkDescription' - The description of the shared custom framework.
--
-- 'status', 'assessmentFrameworkShareRequest_status' - The status of the share request.
--
-- 'id', 'assessmentFrameworkShareRequest_id' - The unique identifier for the share request.
--
-- 'frameworkName', 'assessmentFrameworkShareRequest_frameworkName' - The name of the custom framework that the share request is for.
--
-- 'frameworkId', 'assessmentFrameworkShareRequest_frameworkId' - The unique identifier for the shared custom framework.
--
-- 'lastUpdated', 'assessmentFrameworkShareRequest_lastUpdated' - Specifies when the share request was last updated.
--
-- 'comment', 'assessmentFrameworkShareRequest_comment' - An optional comment from the sender about the share request.
--
-- 'creationTime', 'assessmentFrameworkShareRequest_creationTime' - The time when the share request was created.
--
-- 'destinationRegion', 'assessmentFrameworkShareRequest_destinationRegion' - The Amazon Web Services Region of the recipient.
--
-- 'complianceType', 'assessmentFrameworkShareRequest_complianceType' - The compliance type that the shared custom framework supports, such as
-- CIS or HIPAA.
--
-- 'standardControlsCount', 'assessmentFrameworkShareRequest_standardControlsCount' - The number of standard controls that are part of the shared custom
-- framework.
--
-- 'sourceAccount', 'assessmentFrameworkShareRequest_sourceAccount' - The Amazon Web Services account of the sender.
newAssessmentFrameworkShareRequest ::
  AssessmentFrameworkShareRequest
newAssessmentFrameworkShareRequest =
  AssessmentFrameworkShareRequest'
    { customControlsCount =
        Prelude.Nothing,
      destinationAccount = Prelude.Nothing,
      expirationTime = Prelude.Nothing,
      frameworkDescription = Prelude.Nothing,
      status = Prelude.Nothing,
      id = Prelude.Nothing,
      frameworkName = Prelude.Nothing,
      frameworkId = Prelude.Nothing,
      lastUpdated = Prelude.Nothing,
      comment = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      destinationRegion = Prelude.Nothing,
      complianceType = Prelude.Nothing,
      standardControlsCount = Prelude.Nothing,
      sourceAccount = Prelude.Nothing
    }

-- | The number of custom controls that are part of the shared custom
-- framework.
assessmentFrameworkShareRequest_customControlsCount :: Lens.Lens' AssessmentFrameworkShareRequest (Prelude.Maybe Prelude.Int)
assessmentFrameworkShareRequest_customControlsCount = Lens.lens (\AssessmentFrameworkShareRequest' {customControlsCount} -> customControlsCount) (\s@AssessmentFrameworkShareRequest' {} a -> s {customControlsCount = a} :: AssessmentFrameworkShareRequest)

-- | The Amazon Web Services account of the recipient.
assessmentFrameworkShareRequest_destinationAccount :: Lens.Lens' AssessmentFrameworkShareRequest (Prelude.Maybe Prelude.Text)
assessmentFrameworkShareRequest_destinationAccount = Lens.lens (\AssessmentFrameworkShareRequest' {destinationAccount} -> destinationAccount) (\s@AssessmentFrameworkShareRequest' {} a -> s {destinationAccount = a} :: AssessmentFrameworkShareRequest)

-- | The time when the share request expires.
assessmentFrameworkShareRequest_expirationTime :: Lens.Lens' AssessmentFrameworkShareRequest (Prelude.Maybe Prelude.UTCTime)
assessmentFrameworkShareRequest_expirationTime = Lens.lens (\AssessmentFrameworkShareRequest' {expirationTime} -> expirationTime) (\s@AssessmentFrameworkShareRequest' {} a -> s {expirationTime = a} :: AssessmentFrameworkShareRequest) Prelude.. Lens.mapping Core._Time

-- | The description of the shared custom framework.
assessmentFrameworkShareRequest_frameworkDescription :: Lens.Lens' AssessmentFrameworkShareRequest (Prelude.Maybe Prelude.Text)
assessmentFrameworkShareRequest_frameworkDescription = Lens.lens (\AssessmentFrameworkShareRequest' {frameworkDescription} -> frameworkDescription) (\s@AssessmentFrameworkShareRequest' {} a -> s {frameworkDescription = a} :: AssessmentFrameworkShareRequest)

-- | The status of the share request.
assessmentFrameworkShareRequest_status :: Lens.Lens' AssessmentFrameworkShareRequest (Prelude.Maybe ShareRequestStatus)
assessmentFrameworkShareRequest_status = Lens.lens (\AssessmentFrameworkShareRequest' {status} -> status) (\s@AssessmentFrameworkShareRequest' {} a -> s {status = a} :: AssessmentFrameworkShareRequest)

-- | The unique identifier for the share request.
assessmentFrameworkShareRequest_id :: Lens.Lens' AssessmentFrameworkShareRequest (Prelude.Maybe Prelude.Text)
assessmentFrameworkShareRequest_id = Lens.lens (\AssessmentFrameworkShareRequest' {id} -> id) (\s@AssessmentFrameworkShareRequest' {} a -> s {id = a} :: AssessmentFrameworkShareRequest)

-- | The name of the custom framework that the share request is for.
assessmentFrameworkShareRequest_frameworkName :: Lens.Lens' AssessmentFrameworkShareRequest (Prelude.Maybe Prelude.Text)
assessmentFrameworkShareRequest_frameworkName = Lens.lens (\AssessmentFrameworkShareRequest' {frameworkName} -> frameworkName) (\s@AssessmentFrameworkShareRequest' {} a -> s {frameworkName = a} :: AssessmentFrameworkShareRequest)

-- | The unique identifier for the shared custom framework.
assessmentFrameworkShareRequest_frameworkId :: Lens.Lens' AssessmentFrameworkShareRequest (Prelude.Maybe Prelude.Text)
assessmentFrameworkShareRequest_frameworkId = Lens.lens (\AssessmentFrameworkShareRequest' {frameworkId} -> frameworkId) (\s@AssessmentFrameworkShareRequest' {} a -> s {frameworkId = a} :: AssessmentFrameworkShareRequest)

-- | Specifies when the share request was last updated.
assessmentFrameworkShareRequest_lastUpdated :: Lens.Lens' AssessmentFrameworkShareRequest (Prelude.Maybe Prelude.UTCTime)
assessmentFrameworkShareRequest_lastUpdated = Lens.lens (\AssessmentFrameworkShareRequest' {lastUpdated} -> lastUpdated) (\s@AssessmentFrameworkShareRequest' {} a -> s {lastUpdated = a} :: AssessmentFrameworkShareRequest) Prelude.. Lens.mapping Core._Time

-- | An optional comment from the sender about the share request.
assessmentFrameworkShareRequest_comment :: Lens.Lens' AssessmentFrameworkShareRequest (Prelude.Maybe Prelude.Text)
assessmentFrameworkShareRequest_comment = Lens.lens (\AssessmentFrameworkShareRequest' {comment} -> comment) (\s@AssessmentFrameworkShareRequest' {} a -> s {comment = a} :: AssessmentFrameworkShareRequest)

-- | The time when the share request was created.
assessmentFrameworkShareRequest_creationTime :: Lens.Lens' AssessmentFrameworkShareRequest (Prelude.Maybe Prelude.UTCTime)
assessmentFrameworkShareRequest_creationTime = Lens.lens (\AssessmentFrameworkShareRequest' {creationTime} -> creationTime) (\s@AssessmentFrameworkShareRequest' {} a -> s {creationTime = a} :: AssessmentFrameworkShareRequest) Prelude.. Lens.mapping Core._Time

-- | The Amazon Web Services Region of the recipient.
assessmentFrameworkShareRequest_destinationRegion :: Lens.Lens' AssessmentFrameworkShareRequest (Prelude.Maybe Prelude.Text)
assessmentFrameworkShareRequest_destinationRegion = Lens.lens (\AssessmentFrameworkShareRequest' {destinationRegion} -> destinationRegion) (\s@AssessmentFrameworkShareRequest' {} a -> s {destinationRegion = a} :: AssessmentFrameworkShareRequest)

-- | The compliance type that the shared custom framework supports, such as
-- CIS or HIPAA.
assessmentFrameworkShareRequest_complianceType :: Lens.Lens' AssessmentFrameworkShareRequest (Prelude.Maybe Prelude.Text)
assessmentFrameworkShareRequest_complianceType = Lens.lens (\AssessmentFrameworkShareRequest' {complianceType} -> complianceType) (\s@AssessmentFrameworkShareRequest' {} a -> s {complianceType = a} :: AssessmentFrameworkShareRequest)

-- | The number of standard controls that are part of the shared custom
-- framework.
assessmentFrameworkShareRequest_standardControlsCount :: Lens.Lens' AssessmentFrameworkShareRequest (Prelude.Maybe Prelude.Int)
assessmentFrameworkShareRequest_standardControlsCount = Lens.lens (\AssessmentFrameworkShareRequest' {standardControlsCount} -> standardControlsCount) (\s@AssessmentFrameworkShareRequest' {} a -> s {standardControlsCount = a} :: AssessmentFrameworkShareRequest)

-- | The Amazon Web Services account of the sender.
assessmentFrameworkShareRequest_sourceAccount :: Lens.Lens' AssessmentFrameworkShareRequest (Prelude.Maybe Prelude.Text)
assessmentFrameworkShareRequest_sourceAccount = Lens.lens (\AssessmentFrameworkShareRequest' {sourceAccount} -> sourceAccount) (\s@AssessmentFrameworkShareRequest' {} a -> s {sourceAccount = a} :: AssessmentFrameworkShareRequest)

instance
  Core.FromJSON
    AssessmentFrameworkShareRequest
  where
  parseJSON =
    Core.withObject
      "AssessmentFrameworkShareRequest"
      ( \x ->
          AssessmentFrameworkShareRequest'
            Prelude.<$> (x Core..:? "customControlsCount")
            Prelude.<*> (x Core..:? "destinationAccount")
            Prelude.<*> (x Core..:? "expirationTime")
            Prelude.<*> (x Core..:? "frameworkDescription")
            Prelude.<*> (x Core..:? "status")
            Prelude.<*> (x Core..:? "id")
            Prelude.<*> (x Core..:? "frameworkName")
            Prelude.<*> (x Core..:? "frameworkId")
            Prelude.<*> (x Core..:? "lastUpdated")
            Prelude.<*> (x Core..:? "comment")
            Prelude.<*> (x Core..:? "creationTime")
            Prelude.<*> (x Core..:? "destinationRegion")
            Prelude.<*> (x Core..:? "complianceType")
            Prelude.<*> (x Core..:? "standardControlsCount")
            Prelude.<*> (x Core..:? "sourceAccount")
      )

instance
  Prelude.Hashable
    AssessmentFrameworkShareRequest
  where
  hashWithSalt
    _salt
    AssessmentFrameworkShareRequest' {..} =
      _salt `Prelude.hashWithSalt` customControlsCount
        `Prelude.hashWithSalt` destinationAccount
        `Prelude.hashWithSalt` expirationTime
        `Prelude.hashWithSalt` frameworkDescription
        `Prelude.hashWithSalt` status
        `Prelude.hashWithSalt` id
        `Prelude.hashWithSalt` frameworkName
        `Prelude.hashWithSalt` frameworkId
        `Prelude.hashWithSalt` lastUpdated
        `Prelude.hashWithSalt` comment
        `Prelude.hashWithSalt` creationTime
        `Prelude.hashWithSalt` destinationRegion
        `Prelude.hashWithSalt` complianceType
        `Prelude.hashWithSalt` standardControlsCount
        `Prelude.hashWithSalt` sourceAccount

instance
  Prelude.NFData
    AssessmentFrameworkShareRequest
  where
  rnf AssessmentFrameworkShareRequest' {..} =
    Prelude.rnf customControlsCount
      `Prelude.seq` Prelude.rnf destinationAccount
      `Prelude.seq` Prelude.rnf expirationTime
      `Prelude.seq` Prelude.rnf frameworkDescription
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf frameworkName
      `Prelude.seq` Prelude.rnf frameworkId
      `Prelude.seq` Prelude.rnf lastUpdated
      `Prelude.seq` Prelude.rnf comment
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf destinationRegion
      `Prelude.seq` Prelude.rnf complianceType
      `Prelude.seq` Prelude.rnf standardControlsCount
      `Prelude.seq` Prelude.rnf sourceAccount

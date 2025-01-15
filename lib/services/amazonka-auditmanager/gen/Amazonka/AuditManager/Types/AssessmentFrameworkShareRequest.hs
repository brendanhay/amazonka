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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AuditManager.Types.AssessmentFrameworkShareRequest where

import Amazonka.AuditManager.Types.ShareRequestStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents a share request for a custom framework in Audit Manager.
--
-- /See:/ 'newAssessmentFrameworkShareRequest' smart constructor.
data AssessmentFrameworkShareRequest = AssessmentFrameworkShareRequest'
  { -- | An optional comment from the sender about the share request.
    comment :: Prelude.Maybe Prelude.Text,
    -- | The compliance type that the shared custom framework supports, such as
    -- CIS or HIPAA.
    complianceType :: Prelude.Maybe Prelude.Text,
    -- | The time when the share request was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The number of custom controls that are part of the shared custom
    -- framework.
    customControlsCount :: Prelude.Maybe Prelude.Int,
    -- | The Amazon Web Services account of the recipient.
    destinationAccount :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services Region of the recipient.
    destinationRegion :: Prelude.Maybe Prelude.Text,
    -- | The time when the share request expires.
    expirationTime :: Prelude.Maybe Data.POSIX,
    -- | The description of the shared custom framework.
    frameworkDescription :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the shared custom framework.
    frameworkId :: Prelude.Maybe Prelude.Text,
    -- | The name of the custom framework that the share request is for.
    frameworkName :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the share request.
    id :: Prelude.Maybe Prelude.Text,
    -- | Specifies when the share request was last updated.
    lastUpdated :: Prelude.Maybe Data.POSIX,
    -- | The Amazon Web Services account of the sender.
    sourceAccount :: Prelude.Maybe Prelude.Text,
    -- | The number of standard controls that are part of the shared custom
    -- framework.
    standardControlsCount :: Prelude.Maybe Prelude.Int,
    -- | The status of the share request.
    status :: Prelude.Maybe ShareRequestStatus
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
-- 'comment', 'assessmentFrameworkShareRequest_comment' - An optional comment from the sender about the share request.
--
-- 'complianceType', 'assessmentFrameworkShareRequest_complianceType' - The compliance type that the shared custom framework supports, such as
-- CIS or HIPAA.
--
-- 'creationTime', 'assessmentFrameworkShareRequest_creationTime' - The time when the share request was created.
--
-- 'customControlsCount', 'assessmentFrameworkShareRequest_customControlsCount' - The number of custom controls that are part of the shared custom
-- framework.
--
-- 'destinationAccount', 'assessmentFrameworkShareRequest_destinationAccount' - The Amazon Web Services account of the recipient.
--
-- 'destinationRegion', 'assessmentFrameworkShareRequest_destinationRegion' - The Amazon Web Services Region of the recipient.
--
-- 'expirationTime', 'assessmentFrameworkShareRequest_expirationTime' - The time when the share request expires.
--
-- 'frameworkDescription', 'assessmentFrameworkShareRequest_frameworkDescription' - The description of the shared custom framework.
--
-- 'frameworkId', 'assessmentFrameworkShareRequest_frameworkId' - The unique identifier for the shared custom framework.
--
-- 'frameworkName', 'assessmentFrameworkShareRequest_frameworkName' - The name of the custom framework that the share request is for.
--
-- 'id', 'assessmentFrameworkShareRequest_id' - The unique identifier for the share request.
--
-- 'lastUpdated', 'assessmentFrameworkShareRequest_lastUpdated' - Specifies when the share request was last updated.
--
-- 'sourceAccount', 'assessmentFrameworkShareRequest_sourceAccount' - The Amazon Web Services account of the sender.
--
-- 'standardControlsCount', 'assessmentFrameworkShareRequest_standardControlsCount' - The number of standard controls that are part of the shared custom
-- framework.
--
-- 'status', 'assessmentFrameworkShareRequest_status' - The status of the share request.
newAssessmentFrameworkShareRequest ::
  AssessmentFrameworkShareRequest
newAssessmentFrameworkShareRequest =
  AssessmentFrameworkShareRequest'
    { comment =
        Prelude.Nothing,
      complianceType = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      customControlsCount = Prelude.Nothing,
      destinationAccount = Prelude.Nothing,
      destinationRegion = Prelude.Nothing,
      expirationTime = Prelude.Nothing,
      frameworkDescription = Prelude.Nothing,
      frameworkId = Prelude.Nothing,
      frameworkName = Prelude.Nothing,
      id = Prelude.Nothing,
      lastUpdated = Prelude.Nothing,
      sourceAccount = Prelude.Nothing,
      standardControlsCount = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | An optional comment from the sender about the share request.
assessmentFrameworkShareRequest_comment :: Lens.Lens' AssessmentFrameworkShareRequest (Prelude.Maybe Prelude.Text)
assessmentFrameworkShareRequest_comment = Lens.lens (\AssessmentFrameworkShareRequest' {comment} -> comment) (\s@AssessmentFrameworkShareRequest' {} a -> s {comment = a} :: AssessmentFrameworkShareRequest)

-- | The compliance type that the shared custom framework supports, such as
-- CIS or HIPAA.
assessmentFrameworkShareRequest_complianceType :: Lens.Lens' AssessmentFrameworkShareRequest (Prelude.Maybe Prelude.Text)
assessmentFrameworkShareRequest_complianceType = Lens.lens (\AssessmentFrameworkShareRequest' {complianceType} -> complianceType) (\s@AssessmentFrameworkShareRequest' {} a -> s {complianceType = a} :: AssessmentFrameworkShareRequest)

-- | The time when the share request was created.
assessmentFrameworkShareRequest_creationTime :: Lens.Lens' AssessmentFrameworkShareRequest (Prelude.Maybe Prelude.UTCTime)
assessmentFrameworkShareRequest_creationTime = Lens.lens (\AssessmentFrameworkShareRequest' {creationTime} -> creationTime) (\s@AssessmentFrameworkShareRequest' {} a -> s {creationTime = a} :: AssessmentFrameworkShareRequest) Prelude.. Lens.mapping Data._Time

-- | The number of custom controls that are part of the shared custom
-- framework.
assessmentFrameworkShareRequest_customControlsCount :: Lens.Lens' AssessmentFrameworkShareRequest (Prelude.Maybe Prelude.Int)
assessmentFrameworkShareRequest_customControlsCount = Lens.lens (\AssessmentFrameworkShareRequest' {customControlsCount} -> customControlsCount) (\s@AssessmentFrameworkShareRequest' {} a -> s {customControlsCount = a} :: AssessmentFrameworkShareRequest)

-- | The Amazon Web Services account of the recipient.
assessmentFrameworkShareRequest_destinationAccount :: Lens.Lens' AssessmentFrameworkShareRequest (Prelude.Maybe Prelude.Text)
assessmentFrameworkShareRequest_destinationAccount = Lens.lens (\AssessmentFrameworkShareRequest' {destinationAccount} -> destinationAccount) (\s@AssessmentFrameworkShareRequest' {} a -> s {destinationAccount = a} :: AssessmentFrameworkShareRequest)

-- | The Amazon Web Services Region of the recipient.
assessmentFrameworkShareRequest_destinationRegion :: Lens.Lens' AssessmentFrameworkShareRequest (Prelude.Maybe Prelude.Text)
assessmentFrameworkShareRequest_destinationRegion = Lens.lens (\AssessmentFrameworkShareRequest' {destinationRegion} -> destinationRegion) (\s@AssessmentFrameworkShareRequest' {} a -> s {destinationRegion = a} :: AssessmentFrameworkShareRequest)

-- | The time when the share request expires.
assessmentFrameworkShareRequest_expirationTime :: Lens.Lens' AssessmentFrameworkShareRequest (Prelude.Maybe Prelude.UTCTime)
assessmentFrameworkShareRequest_expirationTime = Lens.lens (\AssessmentFrameworkShareRequest' {expirationTime} -> expirationTime) (\s@AssessmentFrameworkShareRequest' {} a -> s {expirationTime = a} :: AssessmentFrameworkShareRequest) Prelude.. Lens.mapping Data._Time

-- | The description of the shared custom framework.
assessmentFrameworkShareRequest_frameworkDescription :: Lens.Lens' AssessmentFrameworkShareRequest (Prelude.Maybe Prelude.Text)
assessmentFrameworkShareRequest_frameworkDescription = Lens.lens (\AssessmentFrameworkShareRequest' {frameworkDescription} -> frameworkDescription) (\s@AssessmentFrameworkShareRequest' {} a -> s {frameworkDescription = a} :: AssessmentFrameworkShareRequest)

-- | The unique identifier for the shared custom framework.
assessmentFrameworkShareRequest_frameworkId :: Lens.Lens' AssessmentFrameworkShareRequest (Prelude.Maybe Prelude.Text)
assessmentFrameworkShareRequest_frameworkId = Lens.lens (\AssessmentFrameworkShareRequest' {frameworkId} -> frameworkId) (\s@AssessmentFrameworkShareRequest' {} a -> s {frameworkId = a} :: AssessmentFrameworkShareRequest)

-- | The name of the custom framework that the share request is for.
assessmentFrameworkShareRequest_frameworkName :: Lens.Lens' AssessmentFrameworkShareRequest (Prelude.Maybe Prelude.Text)
assessmentFrameworkShareRequest_frameworkName = Lens.lens (\AssessmentFrameworkShareRequest' {frameworkName} -> frameworkName) (\s@AssessmentFrameworkShareRequest' {} a -> s {frameworkName = a} :: AssessmentFrameworkShareRequest)

-- | The unique identifier for the share request.
assessmentFrameworkShareRequest_id :: Lens.Lens' AssessmentFrameworkShareRequest (Prelude.Maybe Prelude.Text)
assessmentFrameworkShareRequest_id = Lens.lens (\AssessmentFrameworkShareRequest' {id} -> id) (\s@AssessmentFrameworkShareRequest' {} a -> s {id = a} :: AssessmentFrameworkShareRequest)

-- | Specifies when the share request was last updated.
assessmentFrameworkShareRequest_lastUpdated :: Lens.Lens' AssessmentFrameworkShareRequest (Prelude.Maybe Prelude.UTCTime)
assessmentFrameworkShareRequest_lastUpdated = Lens.lens (\AssessmentFrameworkShareRequest' {lastUpdated} -> lastUpdated) (\s@AssessmentFrameworkShareRequest' {} a -> s {lastUpdated = a} :: AssessmentFrameworkShareRequest) Prelude.. Lens.mapping Data._Time

-- | The Amazon Web Services account of the sender.
assessmentFrameworkShareRequest_sourceAccount :: Lens.Lens' AssessmentFrameworkShareRequest (Prelude.Maybe Prelude.Text)
assessmentFrameworkShareRequest_sourceAccount = Lens.lens (\AssessmentFrameworkShareRequest' {sourceAccount} -> sourceAccount) (\s@AssessmentFrameworkShareRequest' {} a -> s {sourceAccount = a} :: AssessmentFrameworkShareRequest)

-- | The number of standard controls that are part of the shared custom
-- framework.
assessmentFrameworkShareRequest_standardControlsCount :: Lens.Lens' AssessmentFrameworkShareRequest (Prelude.Maybe Prelude.Int)
assessmentFrameworkShareRequest_standardControlsCount = Lens.lens (\AssessmentFrameworkShareRequest' {standardControlsCount} -> standardControlsCount) (\s@AssessmentFrameworkShareRequest' {} a -> s {standardControlsCount = a} :: AssessmentFrameworkShareRequest)

-- | The status of the share request.
assessmentFrameworkShareRequest_status :: Lens.Lens' AssessmentFrameworkShareRequest (Prelude.Maybe ShareRequestStatus)
assessmentFrameworkShareRequest_status = Lens.lens (\AssessmentFrameworkShareRequest' {status} -> status) (\s@AssessmentFrameworkShareRequest' {} a -> s {status = a} :: AssessmentFrameworkShareRequest)

instance
  Data.FromJSON
    AssessmentFrameworkShareRequest
  where
  parseJSON =
    Data.withObject
      "AssessmentFrameworkShareRequest"
      ( \x ->
          AssessmentFrameworkShareRequest'
            Prelude.<$> (x Data..:? "comment")
            Prelude.<*> (x Data..:? "complianceType")
            Prelude.<*> (x Data..:? "creationTime")
            Prelude.<*> (x Data..:? "customControlsCount")
            Prelude.<*> (x Data..:? "destinationAccount")
            Prelude.<*> (x Data..:? "destinationRegion")
            Prelude.<*> (x Data..:? "expirationTime")
            Prelude.<*> (x Data..:? "frameworkDescription")
            Prelude.<*> (x Data..:? "frameworkId")
            Prelude.<*> (x Data..:? "frameworkName")
            Prelude.<*> (x Data..:? "id")
            Prelude.<*> (x Data..:? "lastUpdated")
            Prelude.<*> (x Data..:? "sourceAccount")
            Prelude.<*> (x Data..:? "standardControlsCount")
            Prelude.<*> (x Data..:? "status")
      )

instance
  Prelude.Hashable
    AssessmentFrameworkShareRequest
  where
  hashWithSalt
    _salt
    AssessmentFrameworkShareRequest' {..} =
      _salt
        `Prelude.hashWithSalt` comment
        `Prelude.hashWithSalt` complianceType
        `Prelude.hashWithSalt` creationTime
        `Prelude.hashWithSalt` customControlsCount
        `Prelude.hashWithSalt` destinationAccount
        `Prelude.hashWithSalt` destinationRegion
        `Prelude.hashWithSalt` expirationTime
        `Prelude.hashWithSalt` frameworkDescription
        `Prelude.hashWithSalt` frameworkId
        `Prelude.hashWithSalt` frameworkName
        `Prelude.hashWithSalt` id
        `Prelude.hashWithSalt` lastUpdated
        `Prelude.hashWithSalt` sourceAccount
        `Prelude.hashWithSalt` standardControlsCount
        `Prelude.hashWithSalt` status

instance
  Prelude.NFData
    AssessmentFrameworkShareRequest
  where
  rnf AssessmentFrameworkShareRequest' {..} =
    Prelude.rnf comment `Prelude.seq`
      Prelude.rnf complianceType `Prelude.seq`
        Prelude.rnf creationTime `Prelude.seq`
          Prelude.rnf customControlsCount `Prelude.seq`
            Prelude.rnf destinationAccount `Prelude.seq`
              Prelude.rnf destinationRegion `Prelude.seq`
                Prelude.rnf expirationTime `Prelude.seq`
                  Prelude.rnf frameworkDescription `Prelude.seq`
                    Prelude.rnf frameworkId `Prelude.seq`
                      Prelude.rnf frameworkName `Prelude.seq`
                        Prelude.rnf id `Prelude.seq`
                          Prelude.rnf lastUpdated `Prelude.seq`
                            Prelude.rnf sourceAccount `Prelude.seq`
                              Prelude.rnf standardControlsCount `Prelude.seq`
                                Prelude.rnf status

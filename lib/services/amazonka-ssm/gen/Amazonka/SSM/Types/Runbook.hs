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
-- Module      : Amazonka.SSM.Types.Runbook
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.Runbook where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSM.Types.Target
import Amazonka.SSM.Types.TargetLocation

-- | Information about an Automation runbook used in a runbook workflow in
-- Change Manager.
--
-- The Automation runbooks specified for the runbook workflow can\'t run
-- until all required approvals for the change request have been received.
--
-- /See:/ 'newRunbook' smart constructor.
data Runbook = Runbook'
  { -- | The name of the parameter used as the target resource for the
    -- rate-controlled runbook workflow. Required if you specify @Targets@.
    targetParameterName :: Prelude.Maybe Prelude.Text,
    -- | Information about the Amazon Web Services Regions and Amazon Web
    -- Services accounts targeted by the current Runbook operation.
    targetLocations :: Prelude.Maybe (Prelude.NonEmpty TargetLocation),
    -- | The @MaxErrors@ value specified by the user when the execution started,
    -- indicating the maximum number of errors that can occur during the
    -- operation before the updates are stopped or rolled back.
    maxErrors :: Prelude.Maybe Prelude.Text,
    -- | A key-value mapping to target resources that the runbook operation
    -- performs tasks on. Required if you specify @TargetParameterName@.
    targets :: Prelude.Maybe [Target],
    -- | The key-value map of execution parameters, which were supplied when
    -- calling @StartChangeRequestExecution@.
    parameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]),
    -- | The version of the Automation runbook used in a runbook workflow.
    documentVersion :: Prelude.Maybe Prelude.Text,
    -- | The @MaxConcurrency@ value specified by the user when the operation
    -- started, indicating the maximum number of resources that the runbook
    -- operation can run on at the same time.
    maxConcurrency :: Prelude.Maybe Prelude.Text,
    -- | The name of the Automation runbook used in a runbook workflow.
    documentName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Runbook' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetParameterName', 'runbook_targetParameterName' - The name of the parameter used as the target resource for the
-- rate-controlled runbook workflow. Required if you specify @Targets@.
--
-- 'targetLocations', 'runbook_targetLocations' - Information about the Amazon Web Services Regions and Amazon Web
-- Services accounts targeted by the current Runbook operation.
--
-- 'maxErrors', 'runbook_maxErrors' - The @MaxErrors@ value specified by the user when the execution started,
-- indicating the maximum number of errors that can occur during the
-- operation before the updates are stopped or rolled back.
--
-- 'targets', 'runbook_targets' - A key-value mapping to target resources that the runbook operation
-- performs tasks on. Required if you specify @TargetParameterName@.
--
-- 'parameters', 'runbook_parameters' - The key-value map of execution parameters, which were supplied when
-- calling @StartChangeRequestExecution@.
--
-- 'documentVersion', 'runbook_documentVersion' - The version of the Automation runbook used in a runbook workflow.
--
-- 'maxConcurrency', 'runbook_maxConcurrency' - The @MaxConcurrency@ value specified by the user when the operation
-- started, indicating the maximum number of resources that the runbook
-- operation can run on at the same time.
--
-- 'documentName', 'runbook_documentName' - The name of the Automation runbook used in a runbook workflow.
newRunbook ::
  -- | 'documentName'
  Prelude.Text ->
  Runbook
newRunbook pDocumentName_ =
  Runbook'
    { targetParameterName = Prelude.Nothing,
      targetLocations = Prelude.Nothing,
      maxErrors = Prelude.Nothing,
      targets = Prelude.Nothing,
      parameters = Prelude.Nothing,
      documentVersion = Prelude.Nothing,
      maxConcurrency = Prelude.Nothing,
      documentName = pDocumentName_
    }

-- | The name of the parameter used as the target resource for the
-- rate-controlled runbook workflow. Required if you specify @Targets@.
runbook_targetParameterName :: Lens.Lens' Runbook (Prelude.Maybe Prelude.Text)
runbook_targetParameterName = Lens.lens (\Runbook' {targetParameterName} -> targetParameterName) (\s@Runbook' {} a -> s {targetParameterName = a} :: Runbook)

-- | Information about the Amazon Web Services Regions and Amazon Web
-- Services accounts targeted by the current Runbook operation.
runbook_targetLocations :: Lens.Lens' Runbook (Prelude.Maybe (Prelude.NonEmpty TargetLocation))
runbook_targetLocations = Lens.lens (\Runbook' {targetLocations} -> targetLocations) (\s@Runbook' {} a -> s {targetLocations = a} :: Runbook) Prelude.. Lens.mapping Lens.coerced

-- | The @MaxErrors@ value specified by the user when the execution started,
-- indicating the maximum number of errors that can occur during the
-- operation before the updates are stopped or rolled back.
runbook_maxErrors :: Lens.Lens' Runbook (Prelude.Maybe Prelude.Text)
runbook_maxErrors = Lens.lens (\Runbook' {maxErrors} -> maxErrors) (\s@Runbook' {} a -> s {maxErrors = a} :: Runbook)

-- | A key-value mapping to target resources that the runbook operation
-- performs tasks on. Required if you specify @TargetParameterName@.
runbook_targets :: Lens.Lens' Runbook (Prelude.Maybe [Target])
runbook_targets = Lens.lens (\Runbook' {targets} -> targets) (\s@Runbook' {} a -> s {targets = a} :: Runbook) Prelude.. Lens.mapping Lens.coerced

-- | The key-value map of execution parameters, which were supplied when
-- calling @StartChangeRequestExecution@.
runbook_parameters :: Lens.Lens' Runbook (Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]))
runbook_parameters = Lens.lens (\Runbook' {parameters} -> parameters) (\s@Runbook' {} a -> s {parameters = a} :: Runbook) Prelude.. Lens.mapping Lens.coerced

-- | The version of the Automation runbook used in a runbook workflow.
runbook_documentVersion :: Lens.Lens' Runbook (Prelude.Maybe Prelude.Text)
runbook_documentVersion = Lens.lens (\Runbook' {documentVersion} -> documentVersion) (\s@Runbook' {} a -> s {documentVersion = a} :: Runbook)

-- | The @MaxConcurrency@ value specified by the user when the operation
-- started, indicating the maximum number of resources that the runbook
-- operation can run on at the same time.
runbook_maxConcurrency :: Lens.Lens' Runbook (Prelude.Maybe Prelude.Text)
runbook_maxConcurrency = Lens.lens (\Runbook' {maxConcurrency} -> maxConcurrency) (\s@Runbook' {} a -> s {maxConcurrency = a} :: Runbook)

-- | The name of the Automation runbook used in a runbook workflow.
runbook_documentName :: Lens.Lens' Runbook Prelude.Text
runbook_documentName = Lens.lens (\Runbook' {documentName} -> documentName) (\s@Runbook' {} a -> s {documentName = a} :: Runbook)

instance Core.FromJSON Runbook where
  parseJSON =
    Core.withObject
      "Runbook"
      ( \x ->
          Runbook'
            Prelude.<$> (x Core..:? "TargetParameterName")
            Prelude.<*> (x Core..:? "TargetLocations")
            Prelude.<*> (x Core..:? "MaxErrors")
            Prelude.<*> (x Core..:? "Targets" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Parameters" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "DocumentVersion")
            Prelude.<*> (x Core..:? "MaxConcurrency")
            Prelude.<*> (x Core..: "DocumentName")
      )

instance Prelude.Hashable Runbook where
  hashWithSalt _salt Runbook' {..} =
    _salt `Prelude.hashWithSalt` targetParameterName
      `Prelude.hashWithSalt` targetLocations
      `Prelude.hashWithSalt` maxErrors
      `Prelude.hashWithSalt` targets
      `Prelude.hashWithSalt` parameters
      `Prelude.hashWithSalt` documentVersion
      `Prelude.hashWithSalt` maxConcurrency
      `Prelude.hashWithSalt` documentName

instance Prelude.NFData Runbook where
  rnf Runbook' {..} =
    Prelude.rnf targetParameterName
      `Prelude.seq` Prelude.rnf targetLocations
      `Prelude.seq` Prelude.rnf maxErrors
      `Prelude.seq` Prelude.rnf targets
      `Prelude.seq` Prelude.rnf parameters
      `Prelude.seq` Prelude.rnf documentVersion
      `Prelude.seq` Prelude.rnf maxConcurrency
      `Prelude.seq` Prelude.rnf documentName

instance Core.ToJSON Runbook where
  toJSON Runbook' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("TargetParameterName" Core..=)
              Prelude.<$> targetParameterName,
            ("TargetLocations" Core..=)
              Prelude.<$> targetLocations,
            ("MaxErrors" Core..=) Prelude.<$> maxErrors,
            ("Targets" Core..=) Prelude.<$> targets,
            ("Parameters" Core..=) Prelude.<$> parameters,
            ("DocumentVersion" Core..=)
              Prelude.<$> documentVersion,
            ("MaxConcurrency" Core..=)
              Prelude.<$> maxConcurrency,
            Prelude.Just ("DocumentName" Core..= documentName)
          ]
      )

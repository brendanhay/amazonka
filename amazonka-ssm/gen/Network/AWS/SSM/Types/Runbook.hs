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
-- Module      : Network.AWS.SSM.Types.Runbook
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.Runbook where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SSM.Types.Target
import Network.AWS.SSM.Types.TargetLocation

-- | Information about an Automation runbook (Automation document) used in a
-- runbook workflow in Change Manager.
--
-- The Automation runbooks specified for the runbook workflow can\'t run
-- until all required approvals for the change request have been received.
--
-- /See:/ 'newRunbook' smart constructor.
data Runbook = Runbook'
  { -- | The @MaxErrors@ value specified by the user when the execution started,
    -- indicating the maximum number of errors that can occur during the
    -- operation before the updates are stopped or rolled back.
    maxErrors :: Prelude.Maybe Prelude.Text,
    -- | A key-value mapping to target resources that the Runbook operation
    -- performs tasks on. Required if you specify @TargetParameterName@.
    targets :: Prelude.Maybe [Target],
    -- | Information about the AWS Regions and accounts targeted by the current
    -- Runbook operation.
    targetLocations :: Prelude.Maybe (Prelude.NonEmpty TargetLocation),
    -- | The name of the parameter used as the target resource for the
    -- rate-controlled runbook workflow. Required if you specify @Targets@.
    targetParameterName :: Prelude.Maybe Prelude.Text,
    -- | The @MaxConcurrency@ value specified by the user when the operation
    -- started, indicating the maximum number of resources that the runbook
    -- operation can run on at the same time.
    maxConcurrency :: Prelude.Maybe Prelude.Text,
    -- | The version of the Automation runbook (Automation document) used in a
    -- runbook workflow.
    documentVersion :: Prelude.Maybe Prelude.Text,
    -- | The key-value map of execution parameters, which were supplied when
    -- calling @StartChangeRequestExecution@.
    parameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]),
    -- | The name of the Automation runbook (Automation document) used in a
    -- runbook workflow.
    documentName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Runbook' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxErrors', 'runbook_maxErrors' - The @MaxErrors@ value specified by the user when the execution started,
-- indicating the maximum number of errors that can occur during the
-- operation before the updates are stopped or rolled back.
--
-- 'targets', 'runbook_targets' - A key-value mapping to target resources that the Runbook operation
-- performs tasks on. Required if you specify @TargetParameterName@.
--
-- 'targetLocations', 'runbook_targetLocations' - Information about the AWS Regions and accounts targeted by the current
-- Runbook operation.
--
-- 'targetParameterName', 'runbook_targetParameterName' - The name of the parameter used as the target resource for the
-- rate-controlled runbook workflow. Required if you specify @Targets@.
--
-- 'maxConcurrency', 'runbook_maxConcurrency' - The @MaxConcurrency@ value specified by the user when the operation
-- started, indicating the maximum number of resources that the runbook
-- operation can run on at the same time.
--
-- 'documentVersion', 'runbook_documentVersion' - The version of the Automation runbook (Automation document) used in a
-- runbook workflow.
--
-- 'parameters', 'runbook_parameters' - The key-value map of execution parameters, which were supplied when
-- calling @StartChangeRequestExecution@.
--
-- 'documentName', 'runbook_documentName' - The name of the Automation runbook (Automation document) used in a
-- runbook workflow.
newRunbook ::
  -- | 'documentName'
  Prelude.Text ->
  Runbook
newRunbook pDocumentName_ =
  Runbook'
    { maxErrors = Prelude.Nothing,
      targets = Prelude.Nothing,
      targetLocations = Prelude.Nothing,
      targetParameterName = Prelude.Nothing,
      maxConcurrency = Prelude.Nothing,
      documentVersion = Prelude.Nothing,
      parameters = Prelude.Nothing,
      documentName = pDocumentName_
    }

-- | The @MaxErrors@ value specified by the user when the execution started,
-- indicating the maximum number of errors that can occur during the
-- operation before the updates are stopped or rolled back.
runbook_maxErrors :: Lens.Lens' Runbook (Prelude.Maybe Prelude.Text)
runbook_maxErrors = Lens.lens (\Runbook' {maxErrors} -> maxErrors) (\s@Runbook' {} a -> s {maxErrors = a} :: Runbook)

-- | A key-value mapping to target resources that the Runbook operation
-- performs tasks on. Required if you specify @TargetParameterName@.
runbook_targets :: Lens.Lens' Runbook (Prelude.Maybe [Target])
runbook_targets = Lens.lens (\Runbook' {targets} -> targets) (\s@Runbook' {} a -> s {targets = a} :: Runbook) Prelude.. Lens.mapping Prelude._Coerce

-- | Information about the AWS Regions and accounts targeted by the current
-- Runbook operation.
runbook_targetLocations :: Lens.Lens' Runbook (Prelude.Maybe (Prelude.NonEmpty TargetLocation))
runbook_targetLocations = Lens.lens (\Runbook' {targetLocations} -> targetLocations) (\s@Runbook' {} a -> s {targetLocations = a} :: Runbook) Prelude.. Lens.mapping Prelude._Coerce

-- | The name of the parameter used as the target resource for the
-- rate-controlled runbook workflow. Required if you specify @Targets@.
runbook_targetParameterName :: Lens.Lens' Runbook (Prelude.Maybe Prelude.Text)
runbook_targetParameterName = Lens.lens (\Runbook' {targetParameterName} -> targetParameterName) (\s@Runbook' {} a -> s {targetParameterName = a} :: Runbook)

-- | The @MaxConcurrency@ value specified by the user when the operation
-- started, indicating the maximum number of resources that the runbook
-- operation can run on at the same time.
runbook_maxConcurrency :: Lens.Lens' Runbook (Prelude.Maybe Prelude.Text)
runbook_maxConcurrency = Lens.lens (\Runbook' {maxConcurrency} -> maxConcurrency) (\s@Runbook' {} a -> s {maxConcurrency = a} :: Runbook)

-- | The version of the Automation runbook (Automation document) used in a
-- runbook workflow.
runbook_documentVersion :: Lens.Lens' Runbook (Prelude.Maybe Prelude.Text)
runbook_documentVersion = Lens.lens (\Runbook' {documentVersion} -> documentVersion) (\s@Runbook' {} a -> s {documentVersion = a} :: Runbook)

-- | The key-value map of execution parameters, which were supplied when
-- calling @StartChangeRequestExecution@.
runbook_parameters :: Lens.Lens' Runbook (Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]))
runbook_parameters = Lens.lens (\Runbook' {parameters} -> parameters) (\s@Runbook' {} a -> s {parameters = a} :: Runbook) Prelude.. Lens.mapping Prelude._Coerce

-- | The name of the Automation runbook (Automation document) used in a
-- runbook workflow.
runbook_documentName :: Lens.Lens' Runbook Prelude.Text
runbook_documentName = Lens.lens (\Runbook' {documentName} -> documentName) (\s@Runbook' {} a -> s {documentName = a} :: Runbook)

instance Prelude.FromJSON Runbook where
  parseJSON =
    Prelude.withObject
      "Runbook"
      ( \x ->
          Runbook'
            Prelude.<$> (x Prelude..:? "MaxErrors")
            Prelude.<*> (x Prelude..:? "Targets" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..:? "TargetLocations")
            Prelude.<*> (x Prelude..:? "TargetParameterName")
            Prelude.<*> (x Prelude..:? "MaxConcurrency")
            Prelude.<*> (x Prelude..:? "DocumentVersion")
            Prelude.<*> ( x Prelude..:? "Parameters"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..: "DocumentName")
      )

instance Prelude.Hashable Runbook

instance Prelude.NFData Runbook

instance Prelude.ToJSON Runbook where
  toJSON Runbook' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("MaxErrors" Prelude..=) Prelude.<$> maxErrors,
            ("Targets" Prelude..=) Prelude.<$> targets,
            ("TargetLocations" Prelude..=)
              Prelude.<$> targetLocations,
            ("TargetParameterName" Prelude..=)
              Prelude.<$> targetParameterName,
            ("MaxConcurrency" Prelude..=)
              Prelude.<$> maxConcurrency,
            ("DocumentVersion" Prelude..=)
              Prelude.<$> documentVersion,
            ("Parameters" Prelude..=) Prelude.<$> parameters,
            Prelude.Just
              ("DocumentName" Prelude..= documentName)
          ]
      )

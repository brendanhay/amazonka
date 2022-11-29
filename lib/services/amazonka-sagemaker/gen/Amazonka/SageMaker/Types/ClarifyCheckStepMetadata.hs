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
-- Module      : Amazonka.SageMaker.Types.ClarifyCheckStepMetadata
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ClarifyCheckStepMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The container for the metadata for the ClarifyCheck step. For more
-- information, see the topic on
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/build-and-manage-steps.html#step-type-clarify-check ClarifyCheck step>
-- in the /Amazon SageMaker Developer Guide/.
--
-- /See:/ 'newClarifyCheckStepMetadata' smart constructor.
data ClarifyCheckStepMetadata = ClarifyCheckStepMetadata'
  { -- | The model package group name.
    modelPackageGroupName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the check processing job that was run
    -- by this step\'s execution.
    checkJobArn :: Prelude.Maybe Prelude.Text,
    -- | The type of the Clarify Check step
    checkType :: Prelude.Maybe Prelude.Text,
    -- | This flag indicates if a newly calculated baseline can be accessed
    -- through step properties @BaselineUsedForDriftCheckConstraints@ and
    -- @BaselineUsedForDriftCheckStatistics@. If it is set to @False@, the
    -- previous baseline of the configured check type must also be available.
    -- These can be accessed through the @BaselineUsedForDriftCheckConstraints@
    -- property.
    registerNewBaseline :: Prelude.Maybe Prelude.Bool,
    -- | This flag indicates if the drift check against the previous baseline
    -- will be skipped or not. If it is set to @False@, the previous baseline
    -- of the configured check type must be available.
    skipCheck :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon S3 URI of baseline constraints file to be used for the drift
    -- check.
    baselineUsedForDriftCheckConstraints :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 URI of the newly calculated baseline constraints file.
    calculatedBaselineConstraints :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 URI of the violation report if violations are detected.
    violationReport :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ClarifyCheckStepMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'modelPackageGroupName', 'clarifyCheckStepMetadata_modelPackageGroupName' - The model package group name.
--
-- 'checkJobArn', 'clarifyCheckStepMetadata_checkJobArn' - The Amazon Resource Name (ARN) of the check processing job that was run
-- by this step\'s execution.
--
-- 'checkType', 'clarifyCheckStepMetadata_checkType' - The type of the Clarify Check step
--
-- 'registerNewBaseline', 'clarifyCheckStepMetadata_registerNewBaseline' - This flag indicates if a newly calculated baseline can be accessed
-- through step properties @BaselineUsedForDriftCheckConstraints@ and
-- @BaselineUsedForDriftCheckStatistics@. If it is set to @False@, the
-- previous baseline of the configured check type must also be available.
-- These can be accessed through the @BaselineUsedForDriftCheckConstraints@
-- property.
--
-- 'skipCheck', 'clarifyCheckStepMetadata_skipCheck' - This flag indicates if the drift check against the previous baseline
-- will be skipped or not. If it is set to @False@, the previous baseline
-- of the configured check type must be available.
--
-- 'baselineUsedForDriftCheckConstraints', 'clarifyCheckStepMetadata_baselineUsedForDriftCheckConstraints' - The Amazon S3 URI of baseline constraints file to be used for the drift
-- check.
--
-- 'calculatedBaselineConstraints', 'clarifyCheckStepMetadata_calculatedBaselineConstraints' - The Amazon S3 URI of the newly calculated baseline constraints file.
--
-- 'violationReport', 'clarifyCheckStepMetadata_violationReport' - The Amazon S3 URI of the violation report if violations are detected.
newClarifyCheckStepMetadata ::
  ClarifyCheckStepMetadata
newClarifyCheckStepMetadata =
  ClarifyCheckStepMetadata'
    { modelPackageGroupName =
        Prelude.Nothing,
      checkJobArn = Prelude.Nothing,
      checkType = Prelude.Nothing,
      registerNewBaseline = Prelude.Nothing,
      skipCheck = Prelude.Nothing,
      baselineUsedForDriftCheckConstraints =
        Prelude.Nothing,
      calculatedBaselineConstraints = Prelude.Nothing,
      violationReport = Prelude.Nothing
    }

-- | The model package group name.
clarifyCheckStepMetadata_modelPackageGroupName :: Lens.Lens' ClarifyCheckStepMetadata (Prelude.Maybe Prelude.Text)
clarifyCheckStepMetadata_modelPackageGroupName = Lens.lens (\ClarifyCheckStepMetadata' {modelPackageGroupName} -> modelPackageGroupName) (\s@ClarifyCheckStepMetadata' {} a -> s {modelPackageGroupName = a} :: ClarifyCheckStepMetadata)

-- | The Amazon Resource Name (ARN) of the check processing job that was run
-- by this step\'s execution.
clarifyCheckStepMetadata_checkJobArn :: Lens.Lens' ClarifyCheckStepMetadata (Prelude.Maybe Prelude.Text)
clarifyCheckStepMetadata_checkJobArn = Lens.lens (\ClarifyCheckStepMetadata' {checkJobArn} -> checkJobArn) (\s@ClarifyCheckStepMetadata' {} a -> s {checkJobArn = a} :: ClarifyCheckStepMetadata)

-- | The type of the Clarify Check step
clarifyCheckStepMetadata_checkType :: Lens.Lens' ClarifyCheckStepMetadata (Prelude.Maybe Prelude.Text)
clarifyCheckStepMetadata_checkType = Lens.lens (\ClarifyCheckStepMetadata' {checkType} -> checkType) (\s@ClarifyCheckStepMetadata' {} a -> s {checkType = a} :: ClarifyCheckStepMetadata)

-- | This flag indicates if a newly calculated baseline can be accessed
-- through step properties @BaselineUsedForDriftCheckConstraints@ and
-- @BaselineUsedForDriftCheckStatistics@. If it is set to @False@, the
-- previous baseline of the configured check type must also be available.
-- These can be accessed through the @BaselineUsedForDriftCheckConstraints@
-- property.
clarifyCheckStepMetadata_registerNewBaseline :: Lens.Lens' ClarifyCheckStepMetadata (Prelude.Maybe Prelude.Bool)
clarifyCheckStepMetadata_registerNewBaseline = Lens.lens (\ClarifyCheckStepMetadata' {registerNewBaseline} -> registerNewBaseline) (\s@ClarifyCheckStepMetadata' {} a -> s {registerNewBaseline = a} :: ClarifyCheckStepMetadata)

-- | This flag indicates if the drift check against the previous baseline
-- will be skipped or not. If it is set to @False@, the previous baseline
-- of the configured check type must be available.
clarifyCheckStepMetadata_skipCheck :: Lens.Lens' ClarifyCheckStepMetadata (Prelude.Maybe Prelude.Bool)
clarifyCheckStepMetadata_skipCheck = Lens.lens (\ClarifyCheckStepMetadata' {skipCheck} -> skipCheck) (\s@ClarifyCheckStepMetadata' {} a -> s {skipCheck = a} :: ClarifyCheckStepMetadata)

-- | The Amazon S3 URI of baseline constraints file to be used for the drift
-- check.
clarifyCheckStepMetadata_baselineUsedForDriftCheckConstraints :: Lens.Lens' ClarifyCheckStepMetadata (Prelude.Maybe Prelude.Text)
clarifyCheckStepMetadata_baselineUsedForDriftCheckConstraints = Lens.lens (\ClarifyCheckStepMetadata' {baselineUsedForDriftCheckConstraints} -> baselineUsedForDriftCheckConstraints) (\s@ClarifyCheckStepMetadata' {} a -> s {baselineUsedForDriftCheckConstraints = a} :: ClarifyCheckStepMetadata)

-- | The Amazon S3 URI of the newly calculated baseline constraints file.
clarifyCheckStepMetadata_calculatedBaselineConstraints :: Lens.Lens' ClarifyCheckStepMetadata (Prelude.Maybe Prelude.Text)
clarifyCheckStepMetadata_calculatedBaselineConstraints = Lens.lens (\ClarifyCheckStepMetadata' {calculatedBaselineConstraints} -> calculatedBaselineConstraints) (\s@ClarifyCheckStepMetadata' {} a -> s {calculatedBaselineConstraints = a} :: ClarifyCheckStepMetadata)

-- | The Amazon S3 URI of the violation report if violations are detected.
clarifyCheckStepMetadata_violationReport :: Lens.Lens' ClarifyCheckStepMetadata (Prelude.Maybe Prelude.Text)
clarifyCheckStepMetadata_violationReport = Lens.lens (\ClarifyCheckStepMetadata' {violationReport} -> violationReport) (\s@ClarifyCheckStepMetadata' {} a -> s {violationReport = a} :: ClarifyCheckStepMetadata)

instance Core.FromJSON ClarifyCheckStepMetadata where
  parseJSON =
    Core.withObject
      "ClarifyCheckStepMetadata"
      ( \x ->
          ClarifyCheckStepMetadata'
            Prelude.<$> (x Core..:? "ModelPackageGroupName")
            Prelude.<*> (x Core..:? "CheckJobArn")
            Prelude.<*> (x Core..:? "CheckType")
            Prelude.<*> (x Core..:? "RegisterNewBaseline")
            Prelude.<*> (x Core..:? "SkipCheck")
            Prelude.<*> (x Core..:? "BaselineUsedForDriftCheckConstraints")
            Prelude.<*> (x Core..:? "CalculatedBaselineConstraints")
            Prelude.<*> (x Core..:? "ViolationReport")
      )

instance Prelude.Hashable ClarifyCheckStepMetadata where
  hashWithSalt _salt ClarifyCheckStepMetadata' {..} =
    _salt `Prelude.hashWithSalt` modelPackageGroupName
      `Prelude.hashWithSalt` checkJobArn
      `Prelude.hashWithSalt` checkType
      `Prelude.hashWithSalt` registerNewBaseline
      `Prelude.hashWithSalt` skipCheck
      `Prelude.hashWithSalt` baselineUsedForDriftCheckConstraints
      `Prelude.hashWithSalt` calculatedBaselineConstraints
      `Prelude.hashWithSalt` violationReport

instance Prelude.NFData ClarifyCheckStepMetadata where
  rnf ClarifyCheckStepMetadata' {..} =
    Prelude.rnf modelPackageGroupName
      `Prelude.seq` Prelude.rnf checkJobArn
      `Prelude.seq` Prelude.rnf checkType
      `Prelude.seq` Prelude.rnf registerNewBaseline
      `Prelude.seq` Prelude.rnf skipCheck
      `Prelude.seq` Prelude.rnf baselineUsedForDriftCheckConstraints
      `Prelude.seq` Prelude.rnf calculatedBaselineConstraints
      `Prelude.seq` Prelude.rnf violationReport

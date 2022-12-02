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
-- Module      : Amazonka.SageMaker.Types.QualityCheckStepMetadata
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.QualityCheckStepMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Container for the metadata for a Quality check step. For more
-- information, see the topic on
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/build-and-manage-steps.html#step-type-quality-check QualityCheck step>
-- in the /Amazon SageMaker Developer Guide/.
--
-- /See:/ 'newQualityCheckStepMetadata' smart constructor.
data QualityCheckStepMetadata = QualityCheckStepMetadata'
  { -- | The model package group name.
    modelPackageGroupName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 URI of the baseline statistics file used for the drift
    -- check.
    baselineUsedForDriftCheckStatistics :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Quality check processing job that
    -- was run by this step execution.
    checkJobArn :: Prelude.Maybe Prelude.Text,
    -- | The type of the Quality check step.
    checkType :: Prelude.Maybe Prelude.Text,
    -- | This flag indicates if a newly calculated baseline can be accessed
    -- through step properties @BaselineUsedForDriftCheckConstraints@ and
    -- @BaselineUsedForDriftCheckStatistics@. If it is set to @False@, the
    -- previous baseline of the configured check type must also be available.
    -- These can be accessed through the @BaselineUsedForDriftCheckConstraints@
    -- and @ BaselineUsedForDriftCheckStatistics@ properties.
    registerNewBaseline :: Prelude.Maybe Prelude.Bool,
    -- | This flag indicates if the drift check against the previous baseline
    -- will be skipped or not. If it is set to @False@, the previous baseline
    -- of the configured check type must be available.
    skipCheck :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon S3 URI of the baseline constraints file used for the drift
    -- check.
    baselineUsedForDriftCheckConstraints :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 URI of the newly calculated baseline statistics file.
    calculatedBaselineStatistics :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 URI of the newly calculated baseline constraints file.
    calculatedBaselineConstraints :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 URI of violation report if violations are detected.
    violationReport :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'QualityCheckStepMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'modelPackageGroupName', 'qualityCheckStepMetadata_modelPackageGroupName' - The model package group name.
--
-- 'baselineUsedForDriftCheckStatistics', 'qualityCheckStepMetadata_baselineUsedForDriftCheckStatistics' - The Amazon S3 URI of the baseline statistics file used for the drift
-- check.
--
-- 'checkJobArn', 'qualityCheckStepMetadata_checkJobArn' - The Amazon Resource Name (ARN) of the Quality check processing job that
-- was run by this step execution.
--
-- 'checkType', 'qualityCheckStepMetadata_checkType' - The type of the Quality check step.
--
-- 'registerNewBaseline', 'qualityCheckStepMetadata_registerNewBaseline' - This flag indicates if a newly calculated baseline can be accessed
-- through step properties @BaselineUsedForDriftCheckConstraints@ and
-- @BaselineUsedForDriftCheckStatistics@. If it is set to @False@, the
-- previous baseline of the configured check type must also be available.
-- These can be accessed through the @BaselineUsedForDriftCheckConstraints@
-- and @ BaselineUsedForDriftCheckStatistics@ properties.
--
-- 'skipCheck', 'qualityCheckStepMetadata_skipCheck' - This flag indicates if the drift check against the previous baseline
-- will be skipped or not. If it is set to @False@, the previous baseline
-- of the configured check type must be available.
--
-- 'baselineUsedForDriftCheckConstraints', 'qualityCheckStepMetadata_baselineUsedForDriftCheckConstraints' - The Amazon S3 URI of the baseline constraints file used for the drift
-- check.
--
-- 'calculatedBaselineStatistics', 'qualityCheckStepMetadata_calculatedBaselineStatistics' - The Amazon S3 URI of the newly calculated baseline statistics file.
--
-- 'calculatedBaselineConstraints', 'qualityCheckStepMetadata_calculatedBaselineConstraints' - The Amazon S3 URI of the newly calculated baseline constraints file.
--
-- 'violationReport', 'qualityCheckStepMetadata_violationReport' - The Amazon S3 URI of violation report if violations are detected.
newQualityCheckStepMetadata ::
  QualityCheckStepMetadata
newQualityCheckStepMetadata =
  QualityCheckStepMetadata'
    { modelPackageGroupName =
        Prelude.Nothing,
      baselineUsedForDriftCheckStatistics =
        Prelude.Nothing,
      checkJobArn = Prelude.Nothing,
      checkType = Prelude.Nothing,
      registerNewBaseline = Prelude.Nothing,
      skipCheck = Prelude.Nothing,
      baselineUsedForDriftCheckConstraints =
        Prelude.Nothing,
      calculatedBaselineStatistics = Prelude.Nothing,
      calculatedBaselineConstraints = Prelude.Nothing,
      violationReport = Prelude.Nothing
    }

-- | The model package group name.
qualityCheckStepMetadata_modelPackageGroupName :: Lens.Lens' QualityCheckStepMetadata (Prelude.Maybe Prelude.Text)
qualityCheckStepMetadata_modelPackageGroupName = Lens.lens (\QualityCheckStepMetadata' {modelPackageGroupName} -> modelPackageGroupName) (\s@QualityCheckStepMetadata' {} a -> s {modelPackageGroupName = a} :: QualityCheckStepMetadata)

-- | The Amazon S3 URI of the baseline statistics file used for the drift
-- check.
qualityCheckStepMetadata_baselineUsedForDriftCheckStatistics :: Lens.Lens' QualityCheckStepMetadata (Prelude.Maybe Prelude.Text)
qualityCheckStepMetadata_baselineUsedForDriftCheckStatistics = Lens.lens (\QualityCheckStepMetadata' {baselineUsedForDriftCheckStatistics} -> baselineUsedForDriftCheckStatistics) (\s@QualityCheckStepMetadata' {} a -> s {baselineUsedForDriftCheckStatistics = a} :: QualityCheckStepMetadata)

-- | The Amazon Resource Name (ARN) of the Quality check processing job that
-- was run by this step execution.
qualityCheckStepMetadata_checkJobArn :: Lens.Lens' QualityCheckStepMetadata (Prelude.Maybe Prelude.Text)
qualityCheckStepMetadata_checkJobArn = Lens.lens (\QualityCheckStepMetadata' {checkJobArn} -> checkJobArn) (\s@QualityCheckStepMetadata' {} a -> s {checkJobArn = a} :: QualityCheckStepMetadata)

-- | The type of the Quality check step.
qualityCheckStepMetadata_checkType :: Lens.Lens' QualityCheckStepMetadata (Prelude.Maybe Prelude.Text)
qualityCheckStepMetadata_checkType = Lens.lens (\QualityCheckStepMetadata' {checkType} -> checkType) (\s@QualityCheckStepMetadata' {} a -> s {checkType = a} :: QualityCheckStepMetadata)

-- | This flag indicates if a newly calculated baseline can be accessed
-- through step properties @BaselineUsedForDriftCheckConstraints@ and
-- @BaselineUsedForDriftCheckStatistics@. If it is set to @False@, the
-- previous baseline of the configured check type must also be available.
-- These can be accessed through the @BaselineUsedForDriftCheckConstraints@
-- and @ BaselineUsedForDriftCheckStatistics@ properties.
qualityCheckStepMetadata_registerNewBaseline :: Lens.Lens' QualityCheckStepMetadata (Prelude.Maybe Prelude.Bool)
qualityCheckStepMetadata_registerNewBaseline = Lens.lens (\QualityCheckStepMetadata' {registerNewBaseline} -> registerNewBaseline) (\s@QualityCheckStepMetadata' {} a -> s {registerNewBaseline = a} :: QualityCheckStepMetadata)

-- | This flag indicates if the drift check against the previous baseline
-- will be skipped or not. If it is set to @False@, the previous baseline
-- of the configured check type must be available.
qualityCheckStepMetadata_skipCheck :: Lens.Lens' QualityCheckStepMetadata (Prelude.Maybe Prelude.Bool)
qualityCheckStepMetadata_skipCheck = Lens.lens (\QualityCheckStepMetadata' {skipCheck} -> skipCheck) (\s@QualityCheckStepMetadata' {} a -> s {skipCheck = a} :: QualityCheckStepMetadata)

-- | The Amazon S3 URI of the baseline constraints file used for the drift
-- check.
qualityCheckStepMetadata_baselineUsedForDriftCheckConstraints :: Lens.Lens' QualityCheckStepMetadata (Prelude.Maybe Prelude.Text)
qualityCheckStepMetadata_baselineUsedForDriftCheckConstraints = Lens.lens (\QualityCheckStepMetadata' {baselineUsedForDriftCheckConstraints} -> baselineUsedForDriftCheckConstraints) (\s@QualityCheckStepMetadata' {} a -> s {baselineUsedForDriftCheckConstraints = a} :: QualityCheckStepMetadata)

-- | The Amazon S3 URI of the newly calculated baseline statistics file.
qualityCheckStepMetadata_calculatedBaselineStatistics :: Lens.Lens' QualityCheckStepMetadata (Prelude.Maybe Prelude.Text)
qualityCheckStepMetadata_calculatedBaselineStatistics = Lens.lens (\QualityCheckStepMetadata' {calculatedBaselineStatistics} -> calculatedBaselineStatistics) (\s@QualityCheckStepMetadata' {} a -> s {calculatedBaselineStatistics = a} :: QualityCheckStepMetadata)

-- | The Amazon S3 URI of the newly calculated baseline constraints file.
qualityCheckStepMetadata_calculatedBaselineConstraints :: Lens.Lens' QualityCheckStepMetadata (Prelude.Maybe Prelude.Text)
qualityCheckStepMetadata_calculatedBaselineConstraints = Lens.lens (\QualityCheckStepMetadata' {calculatedBaselineConstraints} -> calculatedBaselineConstraints) (\s@QualityCheckStepMetadata' {} a -> s {calculatedBaselineConstraints = a} :: QualityCheckStepMetadata)

-- | The Amazon S3 URI of violation report if violations are detected.
qualityCheckStepMetadata_violationReport :: Lens.Lens' QualityCheckStepMetadata (Prelude.Maybe Prelude.Text)
qualityCheckStepMetadata_violationReport = Lens.lens (\QualityCheckStepMetadata' {violationReport} -> violationReport) (\s@QualityCheckStepMetadata' {} a -> s {violationReport = a} :: QualityCheckStepMetadata)

instance Data.FromJSON QualityCheckStepMetadata where
  parseJSON =
    Data.withObject
      "QualityCheckStepMetadata"
      ( \x ->
          QualityCheckStepMetadata'
            Prelude.<$> (x Data..:? "ModelPackageGroupName")
            Prelude.<*> (x Data..:? "BaselineUsedForDriftCheckStatistics")
            Prelude.<*> (x Data..:? "CheckJobArn")
            Prelude.<*> (x Data..:? "CheckType")
            Prelude.<*> (x Data..:? "RegisterNewBaseline")
            Prelude.<*> (x Data..:? "SkipCheck")
            Prelude.<*> (x Data..:? "BaselineUsedForDriftCheckConstraints")
            Prelude.<*> (x Data..:? "CalculatedBaselineStatistics")
            Prelude.<*> (x Data..:? "CalculatedBaselineConstraints")
            Prelude.<*> (x Data..:? "ViolationReport")
      )

instance Prelude.Hashable QualityCheckStepMetadata where
  hashWithSalt _salt QualityCheckStepMetadata' {..} =
    _salt `Prelude.hashWithSalt` modelPackageGroupName
      `Prelude.hashWithSalt` baselineUsedForDriftCheckStatistics
      `Prelude.hashWithSalt` checkJobArn
      `Prelude.hashWithSalt` checkType
      `Prelude.hashWithSalt` registerNewBaseline
      `Prelude.hashWithSalt` skipCheck
      `Prelude.hashWithSalt` baselineUsedForDriftCheckConstraints
      `Prelude.hashWithSalt` calculatedBaselineStatistics
      `Prelude.hashWithSalt` calculatedBaselineConstraints
      `Prelude.hashWithSalt` violationReport

instance Prelude.NFData QualityCheckStepMetadata where
  rnf QualityCheckStepMetadata' {..} =
    Prelude.rnf modelPackageGroupName
      `Prelude.seq` Prelude.rnf baselineUsedForDriftCheckStatistics
      `Prelude.seq` Prelude.rnf checkJobArn
      `Prelude.seq` Prelude.rnf checkType
      `Prelude.seq` Prelude.rnf registerNewBaseline
      `Prelude.seq` Prelude.rnf skipCheck
      `Prelude.seq` Prelude.rnf baselineUsedForDriftCheckConstraints
      `Prelude.seq` Prelude.rnf calculatedBaselineStatistics
      `Prelude.seq` Prelude.rnf calculatedBaselineConstraints
      `Prelude.seq` Prelude.rnf violationReport

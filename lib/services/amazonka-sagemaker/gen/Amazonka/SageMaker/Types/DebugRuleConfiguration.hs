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
-- Module      : Amazonka.SageMaker.Types.DebugRuleConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.DebugRuleConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.ProcessingInstanceType

-- | Configuration information for SageMaker Debugger rules for debugging. To
-- learn more about how to configure the @DebugRuleConfiguration@
-- parameter, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/debugger-createtrainingjob-api.html Use the SageMaker and Debugger Configuration API Operations to Create, Update, and Debug Your Training Job>.
--
-- /See:/ 'newDebugRuleConfiguration' smart constructor.
data DebugRuleConfiguration = DebugRuleConfiguration'
  { -- | Path to Amazon S3 storage location for rules.
    s3OutputPath :: Prelude.Maybe Prelude.Text,
    -- | The instance type to deploy a Debugger custom rule for debugging a
    -- training job.
    instanceType :: Prelude.Maybe ProcessingInstanceType,
    -- | Runtime configuration for rule container.
    ruleParameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Path to local storage location for output of rules. Defaults to
    -- @\/opt\/ml\/processing\/output\/rule\/@.
    localPath :: Prelude.Maybe Prelude.Text,
    -- | The size, in GB, of the ML storage volume attached to the processing
    -- instance.
    volumeSizeInGB :: Prelude.Maybe Prelude.Natural,
    -- | The name of the rule configuration. It must be unique relative to other
    -- rule configuration names.
    ruleConfigurationName :: Prelude.Text,
    -- | The Amazon Elastic Container (ECR) Image for the managed rule
    -- evaluation.
    ruleEvaluatorImage :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DebugRuleConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3OutputPath', 'debugRuleConfiguration_s3OutputPath' - Path to Amazon S3 storage location for rules.
--
-- 'instanceType', 'debugRuleConfiguration_instanceType' - The instance type to deploy a Debugger custom rule for debugging a
-- training job.
--
-- 'ruleParameters', 'debugRuleConfiguration_ruleParameters' - Runtime configuration for rule container.
--
-- 'localPath', 'debugRuleConfiguration_localPath' - Path to local storage location for output of rules. Defaults to
-- @\/opt\/ml\/processing\/output\/rule\/@.
--
-- 'volumeSizeInGB', 'debugRuleConfiguration_volumeSizeInGB' - The size, in GB, of the ML storage volume attached to the processing
-- instance.
--
-- 'ruleConfigurationName', 'debugRuleConfiguration_ruleConfigurationName' - The name of the rule configuration. It must be unique relative to other
-- rule configuration names.
--
-- 'ruleEvaluatorImage', 'debugRuleConfiguration_ruleEvaluatorImage' - The Amazon Elastic Container (ECR) Image for the managed rule
-- evaluation.
newDebugRuleConfiguration ::
  -- | 'ruleConfigurationName'
  Prelude.Text ->
  -- | 'ruleEvaluatorImage'
  Prelude.Text ->
  DebugRuleConfiguration
newDebugRuleConfiguration
  pRuleConfigurationName_
  pRuleEvaluatorImage_ =
    DebugRuleConfiguration'
      { s3OutputPath =
          Prelude.Nothing,
        instanceType = Prelude.Nothing,
        ruleParameters = Prelude.Nothing,
        localPath = Prelude.Nothing,
        volumeSizeInGB = Prelude.Nothing,
        ruleConfigurationName = pRuleConfigurationName_,
        ruleEvaluatorImage = pRuleEvaluatorImage_
      }

-- | Path to Amazon S3 storage location for rules.
debugRuleConfiguration_s3OutputPath :: Lens.Lens' DebugRuleConfiguration (Prelude.Maybe Prelude.Text)
debugRuleConfiguration_s3OutputPath = Lens.lens (\DebugRuleConfiguration' {s3OutputPath} -> s3OutputPath) (\s@DebugRuleConfiguration' {} a -> s {s3OutputPath = a} :: DebugRuleConfiguration)

-- | The instance type to deploy a Debugger custom rule for debugging a
-- training job.
debugRuleConfiguration_instanceType :: Lens.Lens' DebugRuleConfiguration (Prelude.Maybe ProcessingInstanceType)
debugRuleConfiguration_instanceType = Lens.lens (\DebugRuleConfiguration' {instanceType} -> instanceType) (\s@DebugRuleConfiguration' {} a -> s {instanceType = a} :: DebugRuleConfiguration)

-- | Runtime configuration for rule container.
debugRuleConfiguration_ruleParameters :: Lens.Lens' DebugRuleConfiguration (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
debugRuleConfiguration_ruleParameters = Lens.lens (\DebugRuleConfiguration' {ruleParameters} -> ruleParameters) (\s@DebugRuleConfiguration' {} a -> s {ruleParameters = a} :: DebugRuleConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Path to local storage location for output of rules. Defaults to
-- @\/opt\/ml\/processing\/output\/rule\/@.
debugRuleConfiguration_localPath :: Lens.Lens' DebugRuleConfiguration (Prelude.Maybe Prelude.Text)
debugRuleConfiguration_localPath = Lens.lens (\DebugRuleConfiguration' {localPath} -> localPath) (\s@DebugRuleConfiguration' {} a -> s {localPath = a} :: DebugRuleConfiguration)

-- | The size, in GB, of the ML storage volume attached to the processing
-- instance.
debugRuleConfiguration_volumeSizeInGB :: Lens.Lens' DebugRuleConfiguration (Prelude.Maybe Prelude.Natural)
debugRuleConfiguration_volumeSizeInGB = Lens.lens (\DebugRuleConfiguration' {volumeSizeInGB} -> volumeSizeInGB) (\s@DebugRuleConfiguration' {} a -> s {volumeSizeInGB = a} :: DebugRuleConfiguration)

-- | The name of the rule configuration. It must be unique relative to other
-- rule configuration names.
debugRuleConfiguration_ruleConfigurationName :: Lens.Lens' DebugRuleConfiguration Prelude.Text
debugRuleConfiguration_ruleConfigurationName = Lens.lens (\DebugRuleConfiguration' {ruleConfigurationName} -> ruleConfigurationName) (\s@DebugRuleConfiguration' {} a -> s {ruleConfigurationName = a} :: DebugRuleConfiguration)

-- | The Amazon Elastic Container (ECR) Image for the managed rule
-- evaluation.
debugRuleConfiguration_ruleEvaluatorImage :: Lens.Lens' DebugRuleConfiguration Prelude.Text
debugRuleConfiguration_ruleEvaluatorImage = Lens.lens (\DebugRuleConfiguration' {ruleEvaluatorImage} -> ruleEvaluatorImage) (\s@DebugRuleConfiguration' {} a -> s {ruleEvaluatorImage = a} :: DebugRuleConfiguration)

instance Data.FromJSON DebugRuleConfiguration where
  parseJSON =
    Data.withObject
      "DebugRuleConfiguration"
      ( \x ->
          DebugRuleConfiguration'
            Prelude.<$> (x Data..:? "S3OutputPath")
            Prelude.<*> (x Data..:? "InstanceType")
            Prelude.<*> (x Data..:? "RuleParameters" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "LocalPath")
            Prelude.<*> (x Data..:? "VolumeSizeInGB")
            Prelude.<*> (x Data..: "RuleConfigurationName")
            Prelude.<*> (x Data..: "RuleEvaluatorImage")
      )

instance Prelude.Hashable DebugRuleConfiguration where
  hashWithSalt _salt DebugRuleConfiguration' {..} =
    _salt `Prelude.hashWithSalt` s3OutputPath
      `Prelude.hashWithSalt` instanceType
      `Prelude.hashWithSalt` ruleParameters
      `Prelude.hashWithSalt` localPath
      `Prelude.hashWithSalt` volumeSizeInGB
      `Prelude.hashWithSalt` ruleConfigurationName
      `Prelude.hashWithSalt` ruleEvaluatorImage

instance Prelude.NFData DebugRuleConfiguration where
  rnf DebugRuleConfiguration' {..} =
    Prelude.rnf s3OutputPath
      `Prelude.seq` Prelude.rnf instanceType
      `Prelude.seq` Prelude.rnf ruleParameters
      `Prelude.seq` Prelude.rnf localPath
      `Prelude.seq` Prelude.rnf volumeSizeInGB
      `Prelude.seq` Prelude.rnf ruleConfigurationName
      `Prelude.seq` Prelude.rnf ruleEvaluatorImage

instance Data.ToJSON DebugRuleConfiguration where
  toJSON DebugRuleConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("S3OutputPath" Data..=) Prelude.<$> s3OutputPath,
            ("InstanceType" Data..=) Prelude.<$> instanceType,
            ("RuleParameters" Data..=)
              Prelude.<$> ruleParameters,
            ("LocalPath" Data..=) Prelude.<$> localPath,
            ("VolumeSizeInGB" Data..=)
              Prelude.<$> volumeSizeInGB,
            Prelude.Just
              ( "RuleConfigurationName"
                  Data..= ruleConfigurationName
              ),
            Prelude.Just
              ("RuleEvaluatorImage" Data..= ruleEvaluatorImage)
          ]
      )

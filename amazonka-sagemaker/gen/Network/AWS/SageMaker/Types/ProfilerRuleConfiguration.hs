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
-- Module      : Network.AWS.SageMaker.Types.ProfilerRuleConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ProfilerRuleConfiguration where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.ProcessingInstanceType

-- | Configuration information for profiling rules.
--
-- /See:/ 'newProfilerRuleConfiguration' smart constructor.
data ProfilerRuleConfiguration = ProfilerRuleConfiguration'
  { -- | Runtime configuration for rule container.
    ruleParameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The instance type to deploy a Debugger custom rule for profiling a
    -- training job.
    instanceType :: Prelude.Maybe ProcessingInstanceType,
    -- | Path to Amazon S3 storage location for rules.
    s3OutputPath :: Prelude.Maybe Prelude.Text,
    -- | The size, in GB, of the ML storage volume attached to the processing
    -- instance.
    volumeSizeInGB :: Prelude.Maybe Prelude.Natural,
    -- | Path to local storage location for output of rules. Defaults to
    -- @\/opt\/ml\/processing\/output\/rule\/@.
    localPath :: Prelude.Maybe Prelude.Text,
    -- | The name of the rule configuration. It must be unique relative to other
    -- rule configuration names.
    ruleConfigurationName :: Prelude.Text,
    -- | The Amazon Elastic Container (ECR) Image for the managed rule
    -- evaluation.
    ruleEvaluatorImage :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProfilerRuleConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ruleParameters', 'profilerRuleConfiguration_ruleParameters' - Runtime configuration for rule container.
--
-- 'instanceType', 'profilerRuleConfiguration_instanceType' - The instance type to deploy a Debugger custom rule for profiling a
-- training job.
--
-- 's3OutputPath', 'profilerRuleConfiguration_s3OutputPath' - Path to Amazon S3 storage location for rules.
--
-- 'volumeSizeInGB', 'profilerRuleConfiguration_volumeSizeInGB' - The size, in GB, of the ML storage volume attached to the processing
-- instance.
--
-- 'localPath', 'profilerRuleConfiguration_localPath' - Path to local storage location for output of rules. Defaults to
-- @\/opt\/ml\/processing\/output\/rule\/@.
--
-- 'ruleConfigurationName', 'profilerRuleConfiguration_ruleConfigurationName' - The name of the rule configuration. It must be unique relative to other
-- rule configuration names.
--
-- 'ruleEvaluatorImage', 'profilerRuleConfiguration_ruleEvaluatorImage' - The Amazon Elastic Container (ECR) Image for the managed rule
-- evaluation.
newProfilerRuleConfiguration ::
  -- | 'ruleConfigurationName'
  Prelude.Text ->
  -- | 'ruleEvaluatorImage'
  Prelude.Text ->
  ProfilerRuleConfiguration
newProfilerRuleConfiguration
  pRuleConfigurationName_
  pRuleEvaluatorImage_ =
    ProfilerRuleConfiguration'
      { ruleParameters =
          Prelude.Nothing,
        instanceType = Prelude.Nothing,
        s3OutputPath = Prelude.Nothing,
        volumeSizeInGB = Prelude.Nothing,
        localPath = Prelude.Nothing,
        ruleConfigurationName = pRuleConfigurationName_,
        ruleEvaluatorImage = pRuleEvaluatorImage_
      }

-- | Runtime configuration for rule container.
profilerRuleConfiguration_ruleParameters :: Lens.Lens' ProfilerRuleConfiguration (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
profilerRuleConfiguration_ruleParameters = Lens.lens (\ProfilerRuleConfiguration' {ruleParameters} -> ruleParameters) (\s@ProfilerRuleConfiguration' {} a -> s {ruleParameters = a} :: ProfilerRuleConfiguration) Prelude.. Lens.mapping Lens._Coerce

-- | The instance type to deploy a Debugger custom rule for profiling a
-- training job.
profilerRuleConfiguration_instanceType :: Lens.Lens' ProfilerRuleConfiguration (Prelude.Maybe ProcessingInstanceType)
profilerRuleConfiguration_instanceType = Lens.lens (\ProfilerRuleConfiguration' {instanceType} -> instanceType) (\s@ProfilerRuleConfiguration' {} a -> s {instanceType = a} :: ProfilerRuleConfiguration)

-- | Path to Amazon S3 storage location for rules.
profilerRuleConfiguration_s3OutputPath :: Lens.Lens' ProfilerRuleConfiguration (Prelude.Maybe Prelude.Text)
profilerRuleConfiguration_s3OutputPath = Lens.lens (\ProfilerRuleConfiguration' {s3OutputPath} -> s3OutputPath) (\s@ProfilerRuleConfiguration' {} a -> s {s3OutputPath = a} :: ProfilerRuleConfiguration)

-- | The size, in GB, of the ML storage volume attached to the processing
-- instance.
profilerRuleConfiguration_volumeSizeInGB :: Lens.Lens' ProfilerRuleConfiguration (Prelude.Maybe Prelude.Natural)
profilerRuleConfiguration_volumeSizeInGB = Lens.lens (\ProfilerRuleConfiguration' {volumeSizeInGB} -> volumeSizeInGB) (\s@ProfilerRuleConfiguration' {} a -> s {volumeSizeInGB = a} :: ProfilerRuleConfiguration)

-- | Path to local storage location for output of rules. Defaults to
-- @\/opt\/ml\/processing\/output\/rule\/@.
profilerRuleConfiguration_localPath :: Lens.Lens' ProfilerRuleConfiguration (Prelude.Maybe Prelude.Text)
profilerRuleConfiguration_localPath = Lens.lens (\ProfilerRuleConfiguration' {localPath} -> localPath) (\s@ProfilerRuleConfiguration' {} a -> s {localPath = a} :: ProfilerRuleConfiguration)

-- | The name of the rule configuration. It must be unique relative to other
-- rule configuration names.
profilerRuleConfiguration_ruleConfigurationName :: Lens.Lens' ProfilerRuleConfiguration Prelude.Text
profilerRuleConfiguration_ruleConfigurationName = Lens.lens (\ProfilerRuleConfiguration' {ruleConfigurationName} -> ruleConfigurationName) (\s@ProfilerRuleConfiguration' {} a -> s {ruleConfigurationName = a} :: ProfilerRuleConfiguration)

-- | The Amazon Elastic Container (ECR) Image for the managed rule
-- evaluation.
profilerRuleConfiguration_ruleEvaluatorImage :: Lens.Lens' ProfilerRuleConfiguration Prelude.Text
profilerRuleConfiguration_ruleEvaluatorImage = Lens.lens (\ProfilerRuleConfiguration' {ruleEvaluatorImage} -> ruleEvaluatorImage) (\s@ProfilerRuleConfiguration' {} a -> s {ruleEvaluatorImage = a} :: ProfilerRuleConfiguration)

instance Core.FromJSON ProfilerRuleConfiguration where
  parseJSON =
    Core.withObject
      "ProfilerRuleConfiguration"
      ( \x ->
          ProfilerRuleConfiguration'
            Prelude.<$> (x Core..:? "RuleParameters" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "InstanceType")
            Prelude.<*> (x Core..:? "S3OutputPath")
            Prelude.<*> (x Core..:? "VolumeSizeInGB")
            Prelude.<*> (x Core..:? "LocalPath")
            Prelude.<*> (x Core..: "RuleConfigurationName")
            Prelude.<*> (x Core..: "RuleEvaluatorImage")
      )

instance Prelude.Hashable ProfilerRuleConfiguration

instance Prelude.NFData ProfilerRuleConfiguration

instance Core.ToJSON ProfilerRuleConfiguration where
  toJSON ProfilerRuleConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("RuleParameters" Core..=)
              Prelude.<$> ruleParameters,
            ("InstanceType" Core..=) Prelude.<$> instanceType,
            ("S3OutputPath" Core..=) Prelude.<$> s3OutputPath,
            ("VolumeSizeInGB" Core..=)
              Prelude.<$> volumeSizeInGB,
            ("LocalPath" Core..=) Prelude.<$> localPath,
            Prelude.Just
              ( "RuleConfigurationName"
                  Core..= ruleConfigurationName
              ),
            Prelude.Just
              ("RuleEvaluatorImage" Core..= ruleEvaluatorImage)
          ]
      )

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
import Network.AWS.SageMaker.Types.ProcessingInstanceType

-- | Configuration information for profiling rules.
--
-- /See:/ 'newProfilerRuleConfiguration' smart constructor.
data ProfilerRuleConfiguration = ProfilerRuleConfiguration'
  { -- | Runtime configuration for rule container.
    ruleParameters :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The instance type to deploy a Debugger custom rule for profiling a
    -- training job.
    instanceType :: Core.Maybe ProcessingInstanceType,
    -- | Path to Amazon S3 storage location for rules.
    s3OutputPath :: Core.Maybe Core.Text,
    -- | The size, in GB, of the ML storage volume attached to the processing
    -- instance.
    volumeSizeInGB :: Core.Maybe Core.Natural,
    -- | Path to local storage location for output of rules. Defaults to
    -- @\/opt\/ml\/processing\/output\/rule\/@.
    localPath :: Core.Maybe Core.Text,
    -- | The name of the rule configuration. It must be unique relative to other
    -- rule configuration names.
    ruleConfigurationName :: Core.Text,
    -- | The Amazon Elastic Container (ECR) Image for the managed rule
    -- evaluation.
    ruleEvaluatorImage :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'ruleEvaluatorImage'
  Core.Text ->
  ProfilerRuleConfiguration
newProfilerRuleConfiguration
  pRuleConfigurationName_
  pRuleEvaluatorImage_ =
    ProfilerRuleConfiguration'
      { ruleParameters =
          Core.Nothing,
        instanceType = Core.Nothing,
        s3OutputPath = Core.Nothing,
        volumeSizeInGB = Core.Nothing,
        localPath = Core.Nothing,
        ruleConfigurationName = pRuleConfigurationName_,
        ruleEvaluatorImage = pRuleEvaluatorImage_
      }

-- | Runtime configuration for rule container.
profilerRuleConfiguration_ruleParameters :: Lens.Lens' ProfilerRuleConfiguration (Core.Maybe (Core.HashMap Core.Text Core.Text))
profilerRuleConfiguration_ruleParameters = Lens.lens (\ProfilerRuleConfiguration' {ruleParameters} -> ruleParameters) (\s@ProfilerRuleConfiguration' {} a -> s {ruleParameters = a} :: ProfilerRuleConfiguration) Core.. Lens.mapping Lens._Coerce

-- | The instance type to deploy a Debugger custom rule for profiling a
-- training job.
profilerRuleConfiguration_instanceType :: Lens.Lens' ProfilerRuleConfiguration (Core.Maybe ProcessingInstanceType)
profilerRuleConfiguration_instanceType = Lens.lens (\ProfilerRuleConfiguration' {instanceType} -> instanceType) (\s@ProfilerRuleConfiguration' {} a -> s {instanceType = a} :: ProfilerRuleConfiguration)

-- | Path to Amazon S3 storage location for rules.
profilerRuleConfiguration_s3OutputPath :: Lens.Lens' ProfilerRuleConfiguration (Core.Maybe Core.Text)
profilerRuleConfiguration_s3OutputPath = Lens.lens (\ProfilerRuleConfiguration' {s3OutputPath} -> s3OutputPath) (\s@ProfilerRuleConfiguration' {} a -> s {s3OutputPath = a} :: ProfilerRuleConfiguration)

-- | The size, in GB, of the ML storage volume attached to the processing
-- instance.
profilerRuleConfiguration_volumeSizeInGB :: Lens.Lens' ProfilerRuleConfiguration (Core.Maybe Core.Natural)
profilerRuleConfiguration_volumeSizeInGB = Lens.lens (\ProfilerRuleConfiguration' {volumeSizeInGB} -> volumeSizeInGB) (\s@ProfilerRuleConfiguration' {} a -> s {volumeSizeInGB = a} :: ProfilerRuleConfiguration)

-- | Path to local storage location for output of rules. Defaults to
-- @\/opt\/ml\/processing\/output\/rule\/@.
profilerRuleConfiguration_localPath :: Lens.Lens' ProfilerRuleConfiguration (Core.Maybe Core.Text)
profilerRuleConfiguration_localPath = Lens.lens (\ProfilerRuleConfiguration' {localPath} -> localPath) (\s@ProfilerRuleConfiguration' {} a -> s {localPath = a} :: ProfilerRuleConfiguration)

-- | The name of the rule configuration. It must be unique relative to other
-- rule configuration names.
profilerRuleConfiguration_ruleConfigurationName :: Lens.Lens' ProfilerRuleConfiguration Core.Text
profilerRuleConfiguration_ruleConfigurationName = Lens.lens (\ProfilerRuleConfiguration' {ruleConfigurationName} -> ruleConfigurationName) (\s@ProfilerRuleConfiguration' {} a -> s {ruleConfigurationName = a} :: ProfilerRuleConfiguration)

-- | The Amazon Elastic Container (ECR) Image for the managed rule
-- evaluation.
profilerRuleConfiguration_ruleEvaluatorImage :: Lens.Lens' ProfilerRuleConfiguration Core.Text
profilerRuleConfiguration_ruleEvaluatorImage = Lens.lens (\ProfilerRuleConfiguration' {ruleEvaluatorImage} -> ruleEvaluatorImage) (\s@ProfilerRuleConfiguration' {} a -> s {ruleEvaluatorImage = a} :: ProfilerRuleConfiguration)

instance Core.FromJSON ProfilerRuleConfiguration where
  parseJSON =
    Core.withObject
      "ProfilerRuleConfiguration"
      ( \x ->
          ProfilerRuleConfiguration'
            Core.<$> (x Core..:? "RuleParameters" Core..!= Core.mempty)
            Core.<*> (x Core..:? "InstanceType")
            Core.<*> (x Core..:? "S3OutputPath")
            Core.<*> (x Core..:? "VolumeSizeInGB")
            Core.<*> (x Core..:? "LocalPath")
            Core.<*> (x Core..: "RuleConfigurationName")
            Core.<*> (x Core..: "RuleEvaluatorImage")
      )

instance Core.Hashable ProfilerRuleConfiguration

instance Core.NFData ProfilerRuleConfiguration

instance Core.ToJSON ProfilerRuleConfiguration where
  toJSON ProfilerRuleConfiguration' {..} =
    Core.object
      ( Core.catMaybes
          [ ("RuleParameters" Core..=) Core.<$> ruleParameters,
            ("InstanceType" Core..=) Core.<$> instanceType,
            ("S3OutputPath" Core..=) Core.<$> s3OutputPath,
            ("VolumeSizeInGB" Core..=) Core.<$> volumeSizeInGB,
            ("LocalPath" Core..=) Core.<$> localPath,
            Core.Just
              ( "RuleConfigurationName"
                  Core..= ruleConfigurationName
              ),
            Core.Just
              ("RuleEvaluatorImage" Core..= ruleEvaluatorImage)
          ]
      )

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
-- Module      : Network.AWS.SageMaker.Types.DebugRuleConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.DebugRuleConfiguration where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SageMaker.Types.ProcessingInstanceType

-- | Configuration information for SageMaker Debugger rules for debugging. To
-- learn more about how to configure the @DebugRuleConfiguration@
-- parameter, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/debugger-createtrainingjob-api.html Use the SageMaker and Debugger Configuration API Operations to Create, Update, and Debug Your Training Job>.
--
-- /See:/ 'newDebugRuleConfiguration' smart constructor.
data DebugRuleConfiguration = DebugRuleConfiguration'
  { -- | Runtime configuration for rule container.
    ruleParameters :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The instance type to deploy a Debugger custom rule for debugging a
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
-- Create a value of 'DebugRuleConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ruleParameters', 'debugRuleConfiguration_ruleParameters' - Runtime configuration for rule container.
--
-- 'instanceType', 'debugRuleConfiguration_instanceType' - The instance type to deploy a Debugger custom rule for debugging a
-- training job.
--
-- 's3OutputPath', 'debugRuleConfiguration_s3OutputPath' - Path to Amazon S3 storage location for rules.
--
-- 'volumeSizeInGB', 'debugRuleConfiguration_volumeSizeInGB' - The size, in GB, of the ML storage volume attached to the processing
-- instance.
--
-- 'localPath', 'debugRuleConfiguration_localPath' - Path to local storage location for output of rules. Defaults to
-- @\/opt\/ml\/processing\/output\/rule\/@.
--
-- 'ruleConfigurationName', 'debugRuleConfiguration_ruleConfigurationName' - The name of the rule configuration. It must be unique relative to other
-- rule configuration names.
--
-- 'ruleEvaluatorImage', 'debugRuleConfiguration_ruleEvaluatorImage' - The Amazon Elastic Container (ECR) Image for the managed rule
-- evaluation.
newDebugRuleConfiguration ::
  -- | 'ruleConfigurationName'
  Core.Text ->
  -- | 'ruleEvaluatorImage'
  Core.Text ->
  DebugRuleConfiguration
newDebugRuleConfiguration
  pRuleConfigurationName_
  pRuleEvaluatorImage_ =
    DebugRuleConfiguration'
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
debugRuleConfiguration_ruleParameters :: Lens.Lens' DebugRuleConfiguration (Core.Maybe (Core.HashMap Core.Text Core.Text))
debugRuleConfiguration_ruleParameters = Lens.lens (\DebugRuleConfiguration' {ruleParameters} -> ruleParameters) (\s@DebugRuleConfiguration' {} a -> s {ruleParameters = a} :: DebugRuleConfiguration) Core.. Lens.mapping Lens._Coerce

-- | The instance type to deploy a Debugger custom rule for debugging a
-- training job.
debugRuleConfiguration_instanceType :: Lens.Lens' DebugRuleConfiguration (Core.Maybe ProcessingInstanceType)
debugRuleConfiguration_instanceType = Lens.lens (\DebugRuleConfiguration' {instanceType} -> instanceType) (\s@DebugRuleConfiguration' {} a -> s {instanceType = a} :: DebugRuleConfiguration)

-- | Path to Amazon S3 storage location for rules.
debugRuleConfiguration_s3OutputPath :: Lens.Lens' DebugRuleConfiguration (Core.Maybe Core.Text)
debugRuleConfiguration_s3OutputPath = Lens.lens (\DebugRuleConfiguration' {s3OutputPath} -> s3OutputPath) (\s@DebugRuleConfiguration' {} a -> s {s3OutputPath = a} :: DebugRuleConfiguration)

-- | The size, in GB, of the ML storage volume attached to the processing
-- instance.
debugRuleConfiguration_volumeSizeInGB :: Lens.Lens' DebugRuleConfiguration (Core.Maybe Core.Natural)
debugRuleConfiguration_volumeSizeInGB = Lens.lens (\DebugRuleConfiguration' {volumeSizeInGB} -> volumeSizeInGB) (\s@DebugRuleConfiguration' {} a -> s {volumeSizeInGB = a} :: DebugRuleConfiguration)

-- | Path to local storage location for output of rules. Defaults to
-- @\/opt\/ml\/processing\/output\/rule\/@.
debugRuleConfiguration_localPath :: Lens.Lens' DebugRuleConfiguration (Core.Maybe Core.Text)
debugRuleConfiguration_localPath = Lens.lens (\DebugRuleConfiguration' {localPath} -> localPath) (\s@DebugRuleConfiguration' {} a -> s {localPath = a} :: DebugRuleConfiguration)

-- | The name of the rule configuration. It must be unique relative to other
-- rule configuration names.
debugRuleConfiguration_ruleConfigurationName :: Lens.Lens' DebugRuleConfiguration Core.Text
debugRuleConfiguration_ruleConfigurationName = Lens.lens (\DebugRuleConfiguration' {ruleConfigurationName} -> ruleConfigurationName) (\s@DebugRuleConfiguration' {} a -> s {ruleConfigurationName = a} :: DebugRuleConfiguration)

-- | The Amazon Elastic Container (ECR) Image for the managed rule
-- evaluation.
debugRuleConfiguration_ruleEvaluatorImage :: Lens.Lens' DebugRuleConfiguration Core.Text
debugRuleConfiguration_ruleEvaluatorImage = Lens.lens (\DebugRuleConfiguration' {ruleEvaluatorImage} -> ruleEvaluatorImage) (\s@DebugRuleConfiguration' {} a -> s {ruleEvaluatorImage = a} :: DebugRuleConfiguration)

instance Core.FromJSON DebugRuleConfiguration where
  parseJSON =
    Core.withObject
      "DebugRuleConfiguration"
      ( \x ->
          DebugRuleConfiguration'
            Core.<$> (x Core..:? "RuleParameters" Core..!= Core.mempty)
            Core.<*> (x Core..:? "InstanceType")
            Core.<*> (x Core..:? "S3OutputPath")
            Core.<*> (x Core..:? "VolumeSizeInGB")
            Core.<*> (x Core..:? "LocalPath")
            Core.<*> (x Core..: "RuleConfigurationName")
            Core.<*> (x Core..: "RuleEvaluatorImage")
      )

instance Core.Hashable DebugRuleConfiguration

instance Core.NFData DebugRuleConfiguration

instance Core.ToJSON DebugRuleConfiguration where
  toJSON DebugRuleConfiguration' {..} =
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

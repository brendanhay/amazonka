{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.DebugRuleConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.DebugRuleConfiguration
  ( DebugRuleConfiguration (..),

    -- * Smart constructor
    mkDebugRuleConfiguration,

    -- * Lenses
    drcRuleConfigurationName,
    drcRuleEvaluatorImage,
    drcInstanceType,
    drcLocalPath,
    drcRuleParameters,
    drcS3OutputPath,
    drcVolumeSizeInGB,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.AlgorithmImage as Types
import qualified Network.AWS.SageMaker.Types.ConfigKey as Types
import qualified Network.AWS.SageMaker.Types.ConfigValue as Types
import qualified Network.AWS.SageMaker.Types.DirectoryPath as Types
import qualified Network.AWS.SageMaker.Types.ProcessingInstanceType as Types
import qualified Network.AWS.SageMaker.Types.RuleConfigurationName as Types
import qualified Network.AWS.SageMaker.Types.S3OutputPath as Types

-- | Configuration information for debugging rules.
--
-- /See:/ 'mkDebugRuleConfiguration' smart constructor.
data DebugRuleConfiguration = DebugRuleConfiguration'
  { -- | The name of the rule configuration. It must be unique relative to other rule configuration names.
    ruleConfigurationName :: Types.RuleConfigurationName,
    -- | The Amazon Elastic Container (ECR) Image for the managed rule evaluation.
    ruleEvaluatorImage :: Types.AlgorithmImage,
    -- | The instance type to deploy for a training job.
    instanceType :: Core.Maybe Types.ProcessingInstanceType,
    -- | Path to local storage location for output of rules. Defaults to @/opt/ml/processing/output/rule/@ .
    localPath :: Core.Maybe Types.DirectoryPath,
    -- | Runtime configuration for rule container.
    ruleParameters :: Core.Maybe (Core.HashMap Types.ConfigKey Types.ConfigValue),
    -- | Path to Amazon S3 storage location for rules.
    s3OutputPath :: Core.Maybe Types.S3OutputPath,
    -- | The size, in GB, of the ML storage volume attached to the processing instance.
    volumeSizeInGB :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DebugRuleConfiguration' value with any optional fields omitted.
mkDebugRuleConfiguration ::
  -- | 'ruleConfigurationName'
  Types.RuleConfigurationName ->
  -- | 'ruleEvaluatorImage'
  Types.AlgorithmImage ->
  DebugRuleConfiguration
mkDebugRuleConfiguration ruleConfigurationName ruleEvaluatorImage =
  DebugRuleConfiguration'
    { ruleConfigurationName,
      ruleEvaluatorImage,
      instanceType = Core.Nothing,
      localPath = Core.Nothing,
      ruleParameters = Core.Nothing,
      s3OutputPath = Core.Nothing,
      volumeSizeInGB = Core.Nothing
    }

-- | The name of the rule configuration. It must be unique relative to other rule configuration names.
--
-- /Note:/ Consider using 'ruleConfigurationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcRuleConfigurationName :: Lens.Lens' DebugRuleConfiguration Types.RuleConfigurationName
drcRuleConfigurationName = Lens.field @"ruleConfigurationName"
{-# DEPRECATED drcRuleConfigurationName "Use generic-lens or generic-optics with 'ruleConfigurationName' instead." #-}

-- | The Amazon Elastic Container (ECR) Image for the managed rule evaluation.
--
-- /Note:/ Consider using 'ruleEvaluatorImage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcRuleEvaluatorImage :: Lens.Lens' DebugRuleConfiguration Types.AlgorithmImage
drcRuleEvaluatorImage = Lens.field @"ruleEvaluatorImage"
{-# DEPRECATED drcRuleEvaluatorImage "Use generic-lens or generic-optics with 'ruleEvaluatorImage' instead." #-}

-- | The instance type to deploy for a training job.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcInstanceType :: Lens.Lens' DebugRuleConfiguration (Core.Maybe Types.ProcessingInstanceType)
drcInstanceType = Lens.field @"instanceType"
{-# DEPRECATED drcInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | Path to local storage location for output of rules. Defaults to @/opt/ml/processing/output/rule/@ .
--
-- /Note:/ Consider using 'localPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcLocalPath :: Lens.Lens' DebugRuleConfiguration (Core.Maybe Types.DirectoryPath)
drcLocalPath = Lens.field @"localPath"
{-# DEPRECATED drcLocalPath "Use generic-lens or generic-optics with 'localPath' instead." #-}

-- | Runtime configuration for rule container.
--
-- /Note:/ Consider using 'ruleParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcRuleParameters :: Lens.Lens' DebugRuleConfiguration (Core.Maybe (Core.HashMap Types.ConfigKey Types.ConfigValue))
drcRuleParameters = Lens.field @"ruleParameters"
{-# DEPRECATED drcRuleParameters "Use generic-lens or generic-optics with 'ruleParameters' instead." #-}

-- | Path to Amazon S3 storage location for rules.
--
-- /Note:/ Consider using 's3OutputPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcS3OutputPath :: Lens.Lens' DebugRuleConfiguration (Core.Maybe Types.S3OutputPath)
drcS3OutputPath = Lens.field @"s3OutputPath"
{-# DEPRECATED drcS3OutputPath "Use generic-lens or generic-optics with 's3OutputPath' instead." #-}

-- | The size, in GB, of the ML storage volume attached to the processing instance.
--
-- /Note:/ Consider using 'volumeSizeInGB' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcVolumeSizeInGB :: Lens.Lens' DebugRuleConfiguration (Core.Maybe Core.Natural)
drcVolumeSizeInGB = Lens.field @"volumeSizeInGB"
{-# DEPRECATED drcVolumeSizeInGB "Use generic-lens or generic-optics with 'volumeSizeInGB' instead." #-}

instance Core.FromJSON DebugRuleConfiguration where
  toJSON DebugRuleConfiguration {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("RuleConfigurationName" Core..= ruleConfigurationName),
            Core.Just ("RuleEvaluatorImage" Core..= ruleEvaluatorImage),
            ("InstanceType" Core..=) Core.<$> instanceType,
            ("LocalPath" Core..=) Core.<$> localPath,
            ("RuleParameters" Core..=) Core.<$> ruleParameters,
            ("S3OutputPath" Core..=) Core.<$> s3OutputPath,
            ("VolumeSizeInGB" Core..=) Core.<$> volumeSizeInGB
          ]
      )

instance Core.FromJSON DebugRuleConfiguration where
  parseJSON =
    Core.withObject "DebugRuleConfiguration" Core.$
      \x ->
        DebugRuleConfiguration'
          Core.<$> (x Core..: "RuleConfigurationName")
          Core.<*> (x Core..: "RuleEvaluatorImage")
          Core.<*> (x Core..:? "InstanceType")
          Core.<*> (x Core..:? "LocalPath")
          Core.<*> (x Core..:? "RuleParameters")
          Core.<*> (x Core..:? "S3OutputPath")
          Core.<*> (x Core..:? "VolumeSizeInGB")

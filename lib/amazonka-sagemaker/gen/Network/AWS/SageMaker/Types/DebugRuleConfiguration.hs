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
    drcRuleParameters,
    drcS3OutputPath,
    drcLocalPath,
    drcInstanceType,
    drcVolumeSizeInGB,
    drcRuleEvaluatorImage,
    drcRuleConfigurationName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.ProcessingInstanceType

-- | Configuration information for debugging rules.
--
-- /See:/ 'mkDebugRuleConfiguration' smart constructor.
data DebugRuleConfiguration = DebugRuleConfiguration'
  { -- | Runtime configuration for rule container.
    ruleParameters :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | Path to Amazon S3 storage location for rules.
    s3OutputPath :: Lude.Maybe Lude.Text,
    -- | Path to local storage location for output of rules. Defaults to @/opt/ml/processing/output/rule/@ .
    localPath :: Lude.Maybe Lude.Text,
    -- | The instance type to deploy for a training job.
    instanceType :: Lude.Maybe ProcessingInstanceType,
    -- | The size, in GB, of the ML storage volume attached to the processing instance.
    volumeSizeInGB :: Lude.Maybe Lude.Natural,
    -- | The Amazon Elastic Container (ECR) Image for the managed rule evaluation.
    ruleEvaluatorImage :: Lude.Text,
    -- | The name of the rule configuration. It must be unique relative to other rule configuration names.
    ruleConfigurationName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DebugRuleConfiguration' with the minimum fields required to make a request.
--
-- * 'ruleParameters' - Runtime configuration for rule container.
-- * 's3OutputPath' - Path to Amazon S3 storage location for rules.
-- * 'localPath' - Path to local storage location for output of rules. Defaults to @/opt/ml/processing/output/rule/@ .
-- * 'instanceType' - The instance type to deploy for a training job.
-- * 'volumeSizeInGB' - The size, in GB, of the ML storage volume attached to the processing instance.
-- * 'ruleEvaluatorImage' - The Amazon Elastic Container (ECR) Image for the managed rule evaluation.
-- * 'ruleConfigurationName' - The name of the rule configuration. It must be unique relative to other rule configuration names.
mkDebugRuleConfiguration ::
  -- | 'ruleEvaluatorImage'
  Lude.Text ->
  -- | 'ruleConfigurationName'
  Lude.Text ->
  DebugRuleConfiguration
mkDebugRuleConfiguration
  pRuleEvaluatorImage_
  pRuleConfigurationName_ =
    DebugRuleConfiguration'
      { ruleParameters = Lude.Nothing,
        s3OutputPath = Lude.Nothing,
        localPath = Lude.Nothing,
        instanceType = Lude.Nothing,
        volumeSizeInGB = Lude.Nothing,
        ruleEvaluatorImage = pRuleEvaluatorImage_,
        ruleConfigurationName = pRuleConfigurationName_
      }

-- | Runtime configuration for rule container.
--
-- /Note:/ Consider using 'ruleParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcRuleParameters :: Lens.Lens' DebugRuleConfiguration (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
drcRuleParameters = Lens.lens (ruleParameters :: DebugRuleConfiguration -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {ruleParameters = a} :: DebugRuleConfiguration)
{-# DEPRECATED drcRuleParameters "Use generic-lens or generic-optics with 'ruleParameters' instead." #-}

-- | Path to Amazon S3 storage location for rules.
--
-- /Note:/ Consider using 's3OutputPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcS3OutputPath :: Lens.Lens' DebugRuleConfiguration (Lude.Maybe Lude.Text)
drcS3OutputPath = Lens.lens (s3OutputPath :: DebugRuleConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {s3OutputPath = a} :: DebugRuleConfiguration)
{-# DEPRECATED drcS3OutputPath "Use generic-lens or generic-optics with 's3OutputPath' instead." #-}

-- | Path to local storage location for output of rules. Defaults to @/opt/ml/processing/output/rule/@ .
--
-- /Note:/ Consider using 'localPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcLocalPath :: Lens.Lens' DebugRuleConfiguration (Lude.Maybe Lude.Text)
drcLocalPath = Lens.lens (localPath :: DebugRuleConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {localPath = a} :: DebugRuleConfiguration)
{-# DEPRECATED drcLocalPath "Use generic-lens or generic-optics with 'localPath' instead." #-}

-- | The instance type to deploy for a training job.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcInstanceType :: Lens.Lens' DebugRuleConfiguration (Lude.Maybe ProcessingInstanceType)
drcInstanceType = Lens.lens (instanceType :: DebugRuleConfiguration -> Lude.Maybe ProcessingInstanceType) (\s a -> s {instanceType = a} :: DebugRuleConfiguration)
{-# DEPRECATED drcInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | The size, in GB, of the ML storage volume attached to the processing instance.
--
-- /Note:/ Consider using 'volumeSizeInGB' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcVolumeSizeInGB :: Lens.Lens' DebugRuleConfiguration (Lude.Maybe Lude.Natural)
drcVolumeSizeInGB = Lens.lens (volumeSizeInGB :: DebugRuleConfiguration -> Lude.Maybe Lude.Natural) (\s a -> s {volumeSizeInGB = a} :: DebugRuleConfiguration)
{-# DEPRECATED drcVolumeSizeInGB "Use generic-lens or generic-optics with 'volumeSizeInGB' instead." #-}

-- | The Amazon Elastic Container (ECR) Image for the managed rule evaluation.
--
-- /Note:/ Consider using 'ruleEvaluatorImage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcRuleEvaluatorImage :: Lens.Lens' DebugRuleConfiguration Lude.Text
drcRuleEvaluatorImage = Lens.lens (ruleEvaluatorImage :: DebugRuleConfiguration -> Lude.Text) (\s a -> s {ruleEvaluatorImage = a} :: DebugRuleConfiguration)
{-# DEPRECATED drcRuleEvaluatorImage "Use generic-lens or generic-optics with 'ruleEvaluatorImage' instead." #-}

-- | The name of the rule configuration. It must be unique relative to other rule configuration names.
--
-- /Note:/ Consider using 'ruleConfigurationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcRuleConfigurationName :: Lens.Lens' DebugRuleConfiguration Lude.Text
drcRuleConfigurationName = Lens.lens (ruleConfigurationName :: DebugRuleConfiguration -> Lude.Text) (\s a -> s {ruleConfigurationName = a} :: DebugRuleConfiguration)
{-# DEPRECATED drcRuleConfigurationName "Use generic-lens or generic-optics with 'ruleConfigurationName' instead." #-}

instance Lude.FromJSON DebugRuleConfiguration where
  parseJSON =
    Lude.withObject
      "DebugRuleConfiguration"
      ( \x ->
          DebugRuleConfiguration'
            Lude.<$> (x Lude..:? "RuleParameters" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "S3OutputPath")
            Lude.<*> (x Lude..:? "LocalPath")
            Lude.<*> (x Lude..:? "InstanceType")
            Lude.<*> (x Lude..:? "VolumeSizeInGB")
            Lude.<*> (x Lude..: "RuleEvaluatorImage")
            Lude.<*> (x Lude..: "RuleConfigurationName")
      )

instance Lude.ToJSON DebugRuleConfiguration where
  toJSON DebugRuleConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("RuleParameters" Lude..=) Lude.<$> ruleParameters,
            ("S3OutputPath" Lude..=) Lude.<$> s3OutputPath,
            ("LocalPath" Lude..=) Lude.<$> localPath,
            ("InstanceType" Lude..=) Lude.<$> instanceType,
            ("VolumeSizeInGB" Lude..=) Lude.<$> volumeSizeInGB,
            Lude.Just ("RuleEvaluatorImage" Lude..= ruleEvaluatorImage),
            Lude.Just ("RuleConfigurationName" Lude..= ruleConfigurationName)
          ]
      )

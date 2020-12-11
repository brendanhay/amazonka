-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.RemediationConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.RemediationConfiguration
  ( RemediationConfiguration (..),

    -- * Smart constructor
    mkRemediationConfiguration,

    -- * Lenses
    rcResourceType,
    rcARN,
    rcAutomatic,
    rcCreatedByService,
    rcRetryAttemptSeconds,
    rcExecutionControls,
    rcParameters,
    rcMaximumAutomaticAttempts,
    rcTargetVersion,
    rcConfigRuleName,
    rcTargetType,
    rcTargetId,
  )
where

import Network.AWS.Config.Types.ExecutionControls
import Network.AWS.Config.Types.RemediationParameterValue
import Network.AWS.Config.Types.RemediationTargetType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An object that represents the details about the remediation configuration that includes the remediation action, parameters, and data to execute the action.
--
-- /See:/ 'mkRemediationConfiguration' smart constructor.
data RemediationConfiguration = RemediationConfiguration'
  { resourceType ::
      Lude.Maybe Lude.Text,
    arn :: Lude.Maybe Lude.Text,
    automatic :: Lude.Maybe Lude.Bool,
    createdByService :: Lude.Maybe Lude.Text,
    retryAttemptSeconds ::
      Lude.Maybe Lude.Natural,
    executionControls ::
      Lude.Maybe ExecutionControls,
    parameters ::
      Lude.Maybe
        ( Lude.HashMap
            Lude.Text
            (RemediationParameterValue)
        ),
    maximumAutomaticAttempts ::
      Lude.Maybe Lude.Natural,
    targetVersion :: Lude.Maybe Lude.Text,
    configRuleName :: Lude.Text,
    targetType :: RemediationTargetType,
    targetId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RemediationConfiguration' with the minimum fields required to make a request.
--
-- * 'arn' - Amazon Resource Name (ARN) of remediation configuration.
-- * 'automatic' - The remediation is triggered automatically.
-- * 'configRuleName' - The name of the AWS Config rule.
-- * 'createdByService' - Name of the service that owns the service linked rule, if applicable.
-- * 'executionControls' - An ExecutionControls object.
-- * 'maximumAutomaticAttempts' - The maximum number of failed attempts for auto-remediation. If you do not select a number, the default is 5.
--
-- For example, if you specify MaximumAutomaticAttempts as 5 with RetryAttemptsSeconds as 50 seconds, AWS Config will put a RemediationException on your behalf for the failing resource after the 5th failed attempt within 50 seconds.
-- * 'parameters' - An object of the RemediationParameterValue.
-- * 'resourceType' - The type of a resource.
-- * 'retryAttemptSeconds' - Maximum time in seconds that AWS Config runs auto-remediation. If you do not select a number, the default is 60 seconds.
--
-- For example, if you specify RetryAttemptsSeconds as 50 seconds and MaximumAutomaticAttempts as 5, AWS Config will run auto-remediations 5 times within 50 seconds before throwing an exception.
-- * 'targetId' - Target ID is the name of the public document.
-- * 'targetType' - The type of the target. Target executes remediation. For example, SSM document.
-- * 'targetVersion' - Version of the target. For example, version of the SSM document.
mkRemediationConfiguration ::
  -- | 'configRuleName'
  Lude.Text ->
  -- | 'targetType'
  RemediationTargetType ->
  -- | 'targetId'
  Lude.Text ->
  RemediationConfiguration
mkRemediationConfiguration pConfigRuleName_ pTargetType_ pTargetId_ =
  RemediationConfiguration'
    { resourceType = Lude.Nothing,
      arn = Lude.Nothing,
      automatic = Lude.Nothing,
      createdByService = Lude.Nothing,
      retryAttemptSeconds = Lude.Nothing,
      executionControls = Lude.Nothing,
      parameters = Lude.Nothing,
      maximumAutomaticAttempts = Lude.Nothing,
      targetVersion = Lude.Nothing,
      configRuleName = pConfigRuleName_,
      targetType = pTargetType_,
      targetId = pTargetId_
    }

-- | The type of a resource.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcResourceType :: Lens.Lens' RemediationConfiguration (Lude.Maybe Lude.Text)
rcResourceType = Lens.lens (resourceType :: RemediationConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {resourceType = a} :: RemediationConfiguration)
{-# DEPRECATED rcResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | Amazon Resource Name (ARN) of remediation configuration.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcARN :: Lens.Lens' RemediationConfiguration (Lude.Maybe Lude.Text)
rcARN = Lens.lens (arn :: RemediationConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: RemediationConfiguration)
{-# DEPRECATED rcARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The remediation is triggered automatically.
--
-- /Note:/ Consider using 'automatic' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcAutomatic :: Lens.Lens' RemediationConfiguration (Lude.Maybe Lude.Bool)
rcAutomatic = Lens.lens (automatic :: RemediationConfiguration -> Lude.Maybe Lude.Bool) (\s a -> s {automatic = a} :: RemediationConfiguration)
{-# DEPRECATED rcAutomatic "Use generic-lens or generic-optics with 'automatic' instead." #-}

-- | Name of the service that owns the service linked rule, if applicable.
--
-- /Note:/ Consider using 'createdByService' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcCreatedByService :: Lens.Lens' RemediationConfiguration (Lude.Maybe Lude.Text)
rcCreatedByService = Lens.lens (createdByService :: RemediationConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {createdByService = a} :: RemediationConfiguration)
{-# DEPRECATED rcCreatedByService "Use generic-lens or generic-optics with 'createdByService' instead." #-}

-- | Maximum time in seconds that AWS Config runs auto-remediation. If you do not select a number, the default is 60 seconds.
--
-- For example, if you specify RetryAttemptsSeconds as 50 seconds and MaximumAutomaticAttempts as 5, AWS Config will run auto-remediations 5 times within 50 seconds before throwing an exception.
--
-- /Note:/ Consider using 'retryAttemptSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcRetryAttemptSeconds :: Lens.Lens' RemediationConfiguration (Lude.Maybe Lude.Natural)
rcRetryAttemptSeconds = Lens.lens (retryAttemptSeconds :: RemediationConfiguration -> Lude.Maybe Lude.Natural) (\s a -> s {retryAttemptSeconds = a} :: RemediationConfiguration)
{-# DEPRECATED rcRetryAttemptSeconds "Use generic-lens or generic-optics with 'retryAttemptSeconds' instead." #-}

-- | An ExecutionControls object.
--
-- /Note:/ Consider using 'executionControls' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcExecutionControls :: Lens.Lens' RemediationConfiguration (Lude.Maybe ExecutionControls)
rcExecutionControls = Lens.lens (executionControls :: RemediationConfiguration -> Lude.Maybe ExecutionControls) (\s a -> s {executionControls = a} :: RemediationConfiguration)
{-# DEPRECATED rcExecutionControls "Use generic-lens or generic-optics with 'executionControls' instead." #-}

-- | An object of the RemediationParameterValue.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcParameters :: Lens.Lens' RemediationConfiguration (Lude.Maybe (Lude.HashMap Lude.Text (RemediationParameterValue)))
rcParameters = Lens.lens (parameters :: RemediationConfiguration -> Lude.Maybe (Lude.HashMap Lude.Text (RemediationParameterValue))) (\s a -> s {parameters = a} :: RemediationConfiguration)
{-# DEPRECATED rcParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | The maximum number of failed attempts for auto-remediation. If you do not select a number, the default is 5.
--
-- For example, if you specify MaximumAutomaticAttempts as 5 with RetryAttemptsSeconds as 50 seconds, AWS Config will put a RemediationException on your behalf for the failing resource after the 5th failed attempt within 50 seconds.
--
-- /Note:/ Consider using 'maximumAutomaticAttempts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcMaximumAutomaticAttempts :: Lens.Lens' RemediationConfiguration (Lude.Maybe Lude.Natural)
rcMaximumAutomaticAttempts = Lens.lens (maximumAutomaticAttempts :: RemediationConfiguration -> Lude.Maybe Lude.Natural) (\s a -> s {maximumAutomaticAttempts = a} :: RemediationConfiguration)
{-# DEPRECATED rcMaximumAutomaticAttempts "Use generic-lens or generic-optics with 'maximumAutomaticAttempts' instead." #-}

-- | Version of the target. For example, version of the SSM document.
--
-- /Note:/ Consider using 'targetVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcTargetVersion :: Lens.Lens' RemediationConfiguration (Lude.Maybe Lude.Text)
rcTargetVersion = Lens.lens (targetVersion :: RemediationConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {targetVersion = a} :: RemediationConfiguration)
{-# DEPRECATED rcTargetVersion "Use generic-lens or generic-optics with 'targetVersion' instead." #-}

-- | The name of the AWS Config rule.
--
-- /Note:/ Consider using 'configRuleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcConfigRuleName :: Lens.Lens' RemediationConfiguration Lude.Text
rcConfigRuleName = Lens.lens (configRuleName :: RemediationConfiguration -> Lude.Text) (\s a -> s {configRuleName = a} :: RemediationConfiguration)
{-# DEPRECATED rcConfigRuleName "Use generic-lens or generic-optics with 'configRuleName' instead." #-}

-- | The type of the target. Target executes remediation. For example, SSM document.
--
-- /Note:/ Consider using 'targetType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcTargetType :: Lens.Lens' RemediationConfiguration RemediationTargetType
rcTargetType = Lens.lens (targetType :: RemediationConfiguration -> RemediationTargetType) (\s a -> s {targetType = a} :: RemediationConfiguration)
{-# DEPRECATED rcTargetType "Use generic-lens or generic-optics with 'targetType' instead." #-}

-- | Target ID is the name of the public document.
--
-- /Note:/ Consider using 'targetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcTargetId :: Lens.Lens' RemediationConfiguration Lude.Text
rcTargetId = Lens.lens (targetId :: RemediationConfiguration -> Lude.Text) (\s a -> s {targetId = a} :: RemediationConfiguration)
{-# DEPRECATED rcTargetId "Use generic-lens or generic-optics with 'targetId' instead." #-}

instance Lude.FromJSON RemediationConfiguration where
  parseJSON =
    Lude.withObject
      "RemediationConfiguration"
      ( \x ->
          RemediationConfiguration'
            Lude.<$> (x Lude..:? "ResourceType")
            Lude.<*> (x Lude..:? "Arn")
            Lude.<*> (x Lude..:? "Automatic")
            Lude.<*> (x Lude..:? "CreatedByService")
            Lude.<*> (x Lude..:? "RetryAttemptSeconds")
            Lude.<*> (x Lude..:? "ExecutionControls")
            Lude.<*> (x Lude..:? "Parameters" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "MaximumAutomaticAttempts")
            Lude.<*> (x Lude..:? "TargetVersion")
            Lude.<*> (x Lude..: "ConfigRuleName")
            Lude.<*> (x Lude..: "TargetType")
            Lude.<*> (x Lude..: "TargetId")
      )

instance Lude.ToJSON RemediationConfiguration where
  toJSON RemediationConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ResourceType" Lude..=) Lude.<$> resourceType,
            ("Arn" Lude..=) Lude.<$> arn,
            ("Automatic" Lude..=) Lude.<$> automatic,
            ("CreatedByService" Lude..=) Lude.<$> createdByService,
            ("RetryAttemptSeconds" Lude..=) Lude.<$> retryAttemptSeconds,
            ("ExecutionControls" Lude..=) Lude.<$> executionControls,
            ("Parameters" Lude..=) Lude.<$> parameters,
            ("MaximumAutomaticAttempts" Lude..=)
              Lude.<$> maximumAutomaticAttempts,
            ("TargetVersion" Lude..=) Lude.<$> targetVersion,
            Lude.Just ("ConfigRuleName" Lude..= configRuleName),
            Lude.Just ("TargetType" Lude..= targetType),
            Lude.Just ("TargetId" Lude..= targetId)
          ]
      )

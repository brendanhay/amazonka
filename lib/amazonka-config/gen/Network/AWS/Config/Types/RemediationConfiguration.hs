{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    rcConfigRuleName,
    rcTargetType,
    rcTargetId,
    rcArn,
    rcAutomatic,
    rcCreatedByService,
    rcExecutionControls,
    rcMaximumAutomaticAttempts,
    rcParameters,
    rcResourceType,
    rcRetryAttemptSeconds,
    rcTargetVersion,
  )
where

import qualified Network.AWS.Config.Types.ConfigRuleName as Types
import qualified Network.AWS.Config.Types.ExecutionControls as Types
import qualified Network.AWS.Config.Types.RemediationParameterValue as Types
import qualified Network.AWS.Config.Types.RemediationTargetType as Types
import qualified Network.AWS.Config.Types.String as Types
import qualified Network.AWS.Config.Types.StringWithCharLimit1024 as Types
import qualified Network.AWS.Config.Types.StringWithCharLimit256 as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An object that represents the details about the remediation configuration that includes the remediation action, parameters, and data to execute the action.
--
-- /See:/ 'mkRemediationConfiguration' smart constructor.
data RemediationConfiguration = RemediationConfiguration'
  { -- | The name of the AWS Config rule.
    configRuleName :: Types.ConfigRuleName,
    -- | The type of the target. Target executes remediation. For example, SSM document.
    targetType :: Types.RemediationTargetType,
    -- | Target ID is the name of the public document.
    targetId :: Types.StringWithCharLimit256,
    -- | Amazon Resource Name (ARN) of remediation configuration.
    arn :: Core.Maybe Types.StringWithCharLimit1024,
    -- | The remediation is triggered automatically.
    automatic :: Core.Maybe Core.Bool,
    -- | Name of the service that owns the service linked rule, if applicable.
    createdByService :: Core.Maybe Types.StringWithCharLimit1024,
    -- | An ExecutionControls object.
    executionControls :: Core.Maybe Types.ExecutionControls,
    -- | The maximum number of failed attempts for auto-remediation. If you do not select a number, the default is 5.
    --
    -- For example, if you specify MaximumAutomaticAttempts as 5 with RetryAttemptsSeconds as 50 seconds, AWS Config will put a RemediationException on your behalf for the failing resource after the 5th failed attempt within 50 seconds.
    maximumAutomaticAttempts :: Core.Maybe Core.Natural,
    -- | An object of the RemediationParameterValue.
    parameters :: Core.Maybe (Core.HashMap Types.StringWithCharLimit256 Types.RemediationParameterValue),
    -- | The type of a resource.
    resourceType :: Core.Maybe Types.String,
    -- | Maximum time in seconds that AWS Config runs auto-remediation. If you do not select a number, the default is 60 seconds.
    --
    -- For example, if you specify RetryAttemptsSeconds as 50 seconds and MaximumAutomaticAttempts as 5, AWS Config will run auto-remediations 5 times within 50 seconds before throwing an exception.
    retryAttemptSeconds :: Core.Maybe Core.Natural,
    -- | Version of the target. For example, version of the SSM document.
    targetVersion :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RemediationConfiguration' value with any optional fields omitted.
mkRemediationConfiguration ::
  -- | 'configRuleName'
  Types.ConfigRuleName ->
  -- | 'targetType'
  Types.RemediationTargetType ->
  -- | 'targetId'
  Types.StringWithCharLimit256 ->
  RemediationConfiguration
mkRemediationConfiguration configRuleName targetType targetId =
  RemediationConfiguration'
    { configRuleName,
      targetType,
      targetId,
      arn = Core.Nothing,
      automatic = Core.Nothing,
      createdByService = Core.Nothing,
      executionControls = Core.Nothing,
      maximumAutomaticAttempts = Core.Nothing,
      parameters = Core.Nothing,
      resourceType = Core.Nothing,
      retryAttemptSeconds = Core.Nothing,
      targetVersion = Core.Nothing
    }

-- | The name of the AWS Config rule.
--
-- /Note:/ Consider using 'configRuleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcConfigRuleName :: Lens.Lens' RemediationConfiguration Types.ConfigRuleName
rcConfigRuleName = Lens.field @"configRuleName"
{-# DEPRECATED rcConfigRuleName "Use generic-lens or generic-optics with 'configRuleName' instead." #-}

-- | The type of the target. Target executes remediation. For example, SSM document.
--
-- /Note:/ Consider using 'targetType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcTargetType :: Lens.Lens' RemediationConfiguration Types.RemediationTargetType
rcTargetType = Lens.field @"targetType"
{-# DEPRECATED rcTargetType "Use generic-lens or generic-optics with 'targetType' instead." #-}

-- | Target ID is the name of the public document.
--
-- /Note:/ Consider using 'targetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcTargetId :: Lens.Lens' RemediationConfiguration Types.StringWithCharLimit256
rcTargetId = Lens.field @"targetId"
{-# DEPRECATED rcTargetId "Use generic-lens or generic-optics with 'targetId' instead." #-}

-- | Amazon Resource Name (ARN) of remediation configuration.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcArn :: Lens.Lens' RemediationConfiguration (Core.Maybe Types.StringWithCharLimit1024)
rcArn = Lens.field @"arn"
{-# DEPRECATED rcArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The remediation is triggered automatically.
--
-- /Note:/ Consider using 'automatic' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcAutomatic :: Lens.Lens' RemediationConfiguration (Core.Maybe Core.Bool)
rcAutomatic = Lens.field @"automatic"
{-# DEPRECATED rcAutomatic "Use generic-lens or generic-optics with 'automatic' instead." #-}

-- | Name of the service that owns the service linked rule, if applicable.
--
-- /Note:/ Consider using 'createdByService' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcCreatedByService :: Lens.Lens' RemediationConfiguration (Core.Maybe Types.StringWithCharLimit1024)
rcCreatedByService = Lens.field @"createdByService"
{-# DEPRECATED rcCreatedByService "Use generic-lens or generic-optics with 'createdByService' instead." #-}

-- | An ExecutionControls object.
--
-- /Note:/ Consider using 'executionControls' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcExecutionControls :: Lens.Lens' RemediationConfiguration (Core.Maybe Types.ExecutionControls)
rcExecutionControls = Lens.field @"executionControls"
{-# DEPRECATED rcExecutionControls "Use generic-lens or generic-optics with 'executionControls' instead." #-}

-- | The maximum number of failed attempts for auto-remediation. If you do not select a number, the default is 5.
--
-- For example, if you specify MaximumAutomaticAttempts as 5 with RetryAttemptsSeconds as 50 seconds, AWS Config will put a RemediationException on your behalf for the failing resource after the 5th failed attempt within 50 seconds.
--
-- /Note:/ Consider using 'maximumAutomaticAttempts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcMaximumAutomaticAttempts :: Lens.Lens' RemediationConfiguration (Core.Maybe Core.Natural)
rcMaximumAutomaticAttempts = Lens.field @"maximumAutomaticAttempts"
{-# DEPRECATED rcMaximumAutomaticAttempts "Use generic-lens or generic-optics with 'maximumAutomaticAttempts' instead." #-}

-- | An object of the RemediationParameterValue.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcParameters :: Lens.Lens' RemediationConfiguration (Core.Maybe (Core.HashMap Types.StringWithCharLimit256 Types.RemediationParameterValue))
rcParameters = Lens.field @"parameters"
{-# DEPRECATED rcParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | The type of a resource.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcResourceType :: Lens.Lens' RemediationConfiguration (Core.Maybe Types.String)
rcResourceType = Lens.field @"resourceType"
{-# DEPRECATED rcResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | Maximum time in seconds that AWS Config runs auto-remediation. If you do not select a number, the default is 60 seconds.
--
-- For example, if you specify RetryAttemptsSeconds as 50 seconds and MaximumAutomaticAttempts as 5, AWS Config will run auto-remediations 5 times within 50 seconds before throwing an exception.
--
-- /Note:/ Consider using 'retryAttemptSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcRetryAttemptSeconds :: Lens.Lens' RemediationConfiguration (Core.Maybe Core.Natural)
rcRetryAttemptSeconds = Lens.field @"retryAttemptSeconds"
{-# DEPRECATED rcRetryAttemptSeconds "Use generic-lens or generic-optics with 'retryAttemptSeconds' instead." #-}

-- | Version of the target. For example, version of the SSM document.
--
-- /Note:/ Consider using 'targetVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcTargetVersion :: Lens.Lens' RemediationConfiguration (Core.Maybe Types.String)
rcTargetVersion = Lens.field @"targetVersion"
{-# DEPRECATED rcTargetVersion "Use generic-lens or generic-optics with 'targetVersion' instead." #-}

instance Core.FromJSON RemediationConfiguration where
  toJSON RemediationConfiguration {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ConfigRuleName" Core..= configRuleName),
            Core.Just ("TargetType" Core..= targetType),
            Core.Just ("TargetId" Core..= targetId),
            ("Arn" Core..=) Core.<$> arn,
            ("Automatic" Core..=) Core.<$> automatic,
            ("CreatedByService" Core..=) Core.<$> createdByService,
            ("ExecutionControls" Core..=) Core.<$> executionControls,
            ("MaximumAutomaticAttempts" Core..=)
              Core.<$> maximumAutomaticAttempts,
            ("Parameters" Core..=) Core.<$> parameters,
            ("ResourceType" Core..=) Core.<$> resourceType,
            ("RetryAttemptSeconds" Core..=) Core.<$> retryAttemptSeconds,
            ("TargetVersion" Core..=) Core.<$> targetVersion
          ]
      )

instance Core.FromJSON RemediationConfiguration where
  parseJSON =
    Core.withObject "RemediationConfiguration" Core.$
      \x ->
        RemediationConfiguration'
          Core.<$> (x Core..: "ConfigRuleName")
          Core.<*> (x Core..: "TargetType")
          Core.<*> (x Core..: "TargetId")
          Core.<*> (x Core..:? "Arn")
          Core.<*> (x Core..:? "Automatic")
          Core.<*> (x Core..:? "CreatedByService")
          Core.<*> (x Core..:? "ExecutionControls")
          Core.<*> (x Core..:? "MaximumAutomaticAttempts")
          Core.<*> (x Core..:? "Parameters")
          Core.<*> (x Core..:? "ResourceType")
          Core.<*> (x Core..:? "RetryAttemptSeconds")
          Core.<*> (x Core..:? "TargetVersion")

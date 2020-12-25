{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.ActionExecutionInput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.ActionExecutionInput
  ( ActionExecutionInput (..),

    -- * Smart constructor
    mkActionExecutionInput,

    -- * Lenses
    aeiActionTypeId,
    aeiConfiguration,
    aeiInputArtifacts,
    aeiNamespace,
    aeiRegion,
    aeiResolvedConfiguration,
    aeiRoleArn,
  )
where

import qualified Network.AWS.CodePipeline.Types.AWSRegionName as Types
import qualified Network.AWS.CodePipeline.Types.ActionConfigurationKey as Types
import qualified Network.AWS.CodePipeline.Types.ActionConfigurationValue as Types
import qualified Network.AWS.CodePipeline.Types.ActionTypeId as Types
import qualified Network.AWS.CodePipeline.Types.ArtifactDetail as Types
import qualified Network.AWS.CodePipeline.Types.Namespace as Types
import qualified Network.AWS.CodePipeline.Types.RoleArn as Types
import qualified Network.AWS.CodePipeline.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Input information used for an action execution.
--
-- /See:/ 'mkActionExecutionInput' smart constructor.
data ActionExecutionInput = ActionExecutionInput'
  { actionTypeId :: Core.Maybe Types.ActionTypeId,
    -- | Configuration data for an action execution.
    configuration :: Core.Maybe (Core.HashMap Types.ActionConfigurationKey Types.ActionConfigurationValue),
    -- | Details of input artifacts of the action that correspond to the action execution.
    inputArtifacts :: Core.Maybe [Types.ArtifactDetail],
    -- | The variable namespace associated with the action. All variables produced as output by this action fall under this namespace.
    namespace :: Core.Maybe Types.Namespace,
    -- | The AWS Region for the action, such as us-east-1.
    region :: Core.Maybe Types.AWSRegionName,
    -- | Configuration data for an action execution with all variable references replaced with their real values for the execution.
    resolvedConfiguration :: Core.Maybe (Core.HashMap Types.String Types.String),
    -- | The ARN of the IAM service role that performs the declared action. This is assumed through the roleArn for the pipeline.
    roleArn :: Core.Maybe Types.RoleArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ActionExecutionInput' value with any optional fields omitted.
mkActionExecutionInput ::
  ActionExecutionInput
mkActionExecutionInput =
  ActionExecutionInput'
    { actionTypeId = Core.Nothing,
      configuration = Core.Nothing,
      inputArtifacts = Core.Nothing,
      namespace = Core.Nothing,
      region = Core.Nothing,
      resolvedConfiguration = Core.Nothing,
      roleArn = Core.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'actionTypeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeiActionTypeId :: Lens.Lens' ActionExecutionInput (Core.Maybe Types.ActionTypeId)
aeiActionTypeId = Lens.field @"actionTypeId"
{-# DEPRECATED aeiActionTypeId "Use generic-lens or generic-optics with 'actionTypeId' instead." #-}

-- | Configuration data for an action execution.
--
-- /Note:/ Consider using 'configuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeiConfiguration :: Lens.Lens' ActionExecutionInput (Core.Maybe (Core.HashMap Types.ActionConfigurationKey Types.ActionConfigurationValue))
aeiConfiguration = Lens.field @"configuration"
{-# DEPRECATED aeiConfiguration "Use generic-lens or generic-optics with 'configuration' instead." #-}

-- | Details of input artifacts of the action that correspond to the action execution.
--
-- /Note:/ Consider using 'inputArtifacts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeiInputArtifacts :: Lens.Lens' ActionExecutionInput (Core.Maybe [Types.ArtifactDetail])
aeiInputArtifacts = Lens.field @"inputArtifacts"
{-# DEPRECATED aeiInputArtifacts "Use generic-lens or generic-optics with 'inputArtifacts' instead." #-}

-- | The variable namespace associated with the action. All variables produced as output by this action fall under this namespace.
--
-- /Note:/ Consider using 'namespace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeiNamespace :: Lens.Lens' ActionExecutionInput (Core.Maybe Types.Namespace)
aeiNamespace = Lens.field @"namespace"
{-# DEPRECATED aeiNamespace "Use generic-lens or generic-optics with 'namespace' instead." #-}

-- | The AWS Region for the action, such as us-east-1.
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeiRegion :: Lens.Lens' ActionExecutionInput (Core.Maybe Types.AWSRegionName)
aeiRegion = Lens.field @"region"
{-# DEPRECATED aeiRegion "Use generic-lens or generic-optics with 'region' instead." #-}

-- | Configuration data for an action execution with all variable references replaced with their real values for the execution.
--
-- /Note:/ Consider using 'resolvedConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeiResolvedConfiguration :: Lens.Lens' ActionExecutionInput (Core.Maybe (Core.HashMap Types.String Types.String))
aeiResolvedConfiguration = Lens.field @"resolvedConfiguration"
{-# DEPRECATED aeiResolvedConfiguration "Use generic-lens or generic-optics with 'resolvedConfiguration' instead." #-}

-- | The ARN of the IAM service role that performs the declared action. This is assumed through the roleArn for the pipeline.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeiRoleArn :: Lens.Lens' ActionExecutionInput (Core.Maybe Types.RoleArn)
aeiRoleArn = Lens.field @"roleArn"
{-# DEPRECATED aeiRoleArn "Use generic-lens or generic-optics with 'roleArn' instead." #-}

instance Core.FromJSON ActionExecutionInput where
  parseJSON =
    Core.withObject "ActionExecutionInput" Core.$
      \x ->
        ActionExecutionInput'
          Core.<$> (x Core..:? "actionTypeId")
          Core.<*> (x Core..:? "configuration")
          Core.<*> (x Core..:? "inputArtifacts")
          Core.<*> (x Core..:? "namespace")
          Core.<*> (x Core..:? "region")
          Core.<*> (x Core..:? "resolvedConfiguration")
          Core.<*> (x Core..:? "roleArn")

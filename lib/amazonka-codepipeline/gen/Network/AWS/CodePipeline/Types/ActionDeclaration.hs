{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.ActionDeclaration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.ActionDeclaration
  ( ActionDeclaration (..),

    -- * Smart constructor
    mkActionDeclaration,

    -- * Lenses
    adName,
    adActionTypeId,
    adConfiguration,
    adInputArtifacts,
    adNamespace,
    adOutputArtifacts,
    adRegion,
    adRoleArn,
    adRunOrder,
  )
where

import qualified Network.AWS.CodePipeline.Types.AWSRegionName as Types
import qualified Network.AWS.CodePipeline.Types.ActionConfigurationKey as Types
import qualified Network.AWS.CodePipeline.Types.ActionConfigurationValue as Types
import qualified Network.AWS.CodePipeline.Types.ActionTypeId as Types
import qualified Network.AWS.CodePipeline.Types.InputArtifact as Types
import qualified Network.AWS.CodePipeline.Types.Name as Types
import qualified Network.AWS.CodePipeline.Types.Namespace as Types
import qualified Network.AWS.CodePipeline.Types.OutputArtifact as Types
import qualified Network.AWS.CodePipeline.Types.RoleArn as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents information about an action declaration.
--
-- /See:/ 'mkActionDeclaration' smart constructor.
data ActionDeclaration = ActionDeclaration'
  { -- | The action declaration's name.
    name :: Types.Name,
    -- | Specifies the action type and the provider of the action.
    actionTypeId :: Types.ActionTypeId,
    -- | The action's configuration. These are key-value pairs that specify input values for an action. For more information, see <https://docs.aws.amazon.com/codepipeline/latest/userguide/reference-pipeline-structure.html#action-requirements Action Structure Requirements in CodePipeline> . For the list of configuration properties for the AWS CloudFormation action type in CodePipeline, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/continuous-delivery-codepipeline-action-reference.html Configuration Properties Reference> in the /AWS CloudFormation User Guide/ . For template snippets with examples, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/continuous-delivery-codepipeline-parameter-override-functions.html Using Parameter Override Functions with CodePipeline Pipelines> in the /AWS CloudFormation User Guide/ .
    --
    -- The values can be represented in either JSON or YAML format. For example, the JSON configuration item format is as follows:
    -- /JSON:/
    -- @"Configuration" : { Key : Value },@
    configuration :: Core.Maybe (Core.HashMap Types.ActionConfigurationKey Types.ActionConfigurationValue),
    -- | The name or ID of the artifact consumed by the action, such as a test or build artifact.
    inputArtifacts :: Core.Maybe [Types.InputArtifact],
    -- | The variable namespace associated with the action. All variables produced as output by this action fall under this namespace.
    namespace :: Core.Maybe Types.Namespace,
    -- | The name or ID of the result of the action declaration, such as a test or build artifact.
    outputArtifacts :: Core.Maybe [Types.OutputArtifact],
    -- | The action declaration's AWS Region, such as us-east-1.
    region :: Core.Maybe Types.AWSRegionName,
    -- | The ARN of the IAM service role that performs the declared action. This is assumed through the roleArn for the pipeline.
    roleArn :: Core.Maybe Types.RoleArn,
    -- | The order in which actions are run.
    runOrder :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ActionDeclaration' value with any optional fields omitted.
mkActionDeclaration ::
  -- | 'name'
  Types.Name ->
  -- | 'actionTypeId'
  Types.ActionTypeId ->
  ActionDeclaration
mkActionDeclaration name actionTypeId =
  ActionDeclaration'
    { name,
      actionTypeId,
      configuration = Core.Nothing,
      inputArtifacts = Core.Nothing,
      namespace = Core.Nothing,
      outputArtifacts = Core.Nothing,
      region = Core.Nothing,
      roleArn = Core.Nothing,
      runOrder = Core.Nothing
    }

-- | The action declaration's name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adName :: Lens.Lens' ActionDeclaration Types.Name
adName = Lens.field @"name"
{-# DEPRECATED adName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Specifies the action type and the provider of the action.
--
-- /Note:/ Consider using 'actionTypeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adActionTypeId :: Lens.Lens' ActionDeclaration Types.ActionTypeId
adActionTypeId = Lens.field @"actionTypeId"
{-# DEPRECATED adActionTypeId "Use generic-lens or generic-optics with 'actionTypeId' instead." #-}

-- | The action's configuration. These are key-value pairs that specify input values for an action. For more information, see <https://docs.aws.amazon.com/codepipeline/latest/userguide/reference-pipeline-structure.html#action-requirements Action Structure Requirements in CodePipeline> . For the list of configuration properties for the AWS CloudFormation action type in CodePipeline, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/continuous-delivery-codepipeline-action-reference.html Configuration Properties Reference> in the /AWS CloudFormation User Guide/ . For template snippets with examples, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/continuous-delivery-codepipeline-parameter-override-functions.html Using Parameter Override Functions with CodePipeline Pipelines> in the /AWS CloudFormation User Guide/ .
--
-- The values can be represented in either JSON or YAML format. For example, the JSON configuration item format is as follows:
-- /JSON:/
-- @"Configuration" : { Key : Value },@
--
-- /Note:/ Consider using 'configuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adConfiguration :: Lens.Lens' ActionDeclaration (Core.Maybe (Core.HashMap Types.ActionConfigurationKey Types.ActionConfigurationValue))
adConfiguration = Lens.field @"configuration"
{-# DEPRECATED adConfiguration "Use generic-lens or generic-optics with 'configuration' instead." #-}

-- | The name or ID of the artifact consumed by the action, such as a test or build artifact.
--
-- /Note:/ Consider using 'inputArtifacts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adInputArtifacts :: Lens.Lens' ActionDeclaration (Core.Maybe [Types.InputArtifact])
adInputArtifacts = Lens.field @"inputArtifacts"
{-# DEPRECATED adInputArtifacts "Use generic-lens or generic-optics with 'inputArtifacts' instead." #-}

-- | The variable namespace associated with the action. All variables produced as output by this action fall under this namespace.
--
-- /Note:/ Consider using 'namespace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adNamespace :: Lens.Lens' ActionDeclaration (Core.Maybe Types.Namespace)
adNamespace = Lens.field @"namespace"
{-# DEPRECATED adNamespace "Use generic-lens or generic-optics with 'namespace' instead." #-}

-- | The name or ID of the result of the action declaration, such as a test or build artifact.
--
-- /Note:/ Consider using 'outputArtifacts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adOutputArtifacts :: Lens.Lens' ActionDeclaration (Core.Maybe [Types.OutputArtifact])
adOutputArtifacts = Lens.field @"outputArtifacts"
{-# DEPRECATED adOutputArtifacts "Use generic-lens or generic-optics with 'outputArtifacts' instead." #-}

-- | The action declaration's AWS Region, such as us-east-1.
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adRegion :: Lens.Lens' ActionDeclaration (Core.Maybe Types.AWSRegionName)
adRegion = Lens.field @"region"
{-# DEPRECATED adRegion "Use generic-lens or generic-optics with 'region' instead." #-}

-- | The ARN of the IAM service role that performs the declared action. This is assumed through the roleArn for the pipeline.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adRoleArn :: Lens.Lens' ActionDeclaration (Core.Maybe Types.RoleArn)
adRoleArn = Lens.field @"roleArn"
{-# DEPRECATED adRoleArn "Use generic-lens or generic-optics with 'roleArn' instead." #-}

-- | The order in which actions are run.
--
-- /Note:/ Consider using 'runOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adRunOrder :: Lens.Lens' ActionDeclaration (Core.Maybe Core.Natural)
adRunOrder = Lens.field @"runOrder"
{-# DEPRECATED adRunOrder "Use generic-lens or generic-optics with 'runOrder' instead." #-}

instance Core.FromJSON ActionDeclaration where
  toJSON ActionDeclaration {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("name" Core..= name),
            Core.Just ("actionTypeId" Core..= actionTypeId),
            ("configuration" Core..=) Core.<$> configuration,
            ("inputArtifacts" Core..=) Core.<$> inputArtifacts,
            ("namespace" Core..=) Core.<$> namespace,
            ("outputArtifacts" Core..=) Core.<$> outputArtifacts,
            ("region" Core..=) Core.<$> region,
            ("roleArn" Core..=) Core.<$> roleArn,
            ("runOrder" Core..=) Core.<$> runOrder
          ]
      )

instance Core.FromJSON ActionDeclaration where
  parseJSON =
    Core.withObject "ActionDeclaration" Core.$
      \x ->
        ActionDeclaration'
          Core.<$> (x Core..: "name")
          Core.<*> (x Core..: "actionTypeId")
          Core.<*> (x Core..:? "configuration")
          Core.<*> (x Core..:? "inputArtifacts")
          Core.<*> (x Core..:? "namespace")
          Core.<*> (x Core..:? "outputArtifacts")
          Core.<*> (x Core..:? "region")
          Core.<*> (x Core..:? "roleArn")
          Core.<*> (x Core..:? "runOrder")

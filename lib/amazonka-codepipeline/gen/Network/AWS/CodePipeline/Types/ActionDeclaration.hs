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
    adOutputArtifacts,
    adNamespace,
    adRunOrder,
    adName,
    adRegion,
    adConfiguration,
    adActionTypeId,
    adInputArtifacts,
    adRoleARN,
  )
where

import Network.AWS.CodePipeline.Types.ActionTypeId
import Network.AWS.CodePipeline.Types.InputArtifact
import Network.AWS.CodePipeline.Types.OutputArtifact
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents information about an action declaration.
--
-- /See:/ 'mkActionDeclaration' smart constructor.
data ActionDeclaration = ActionDeclaration'
  { -- | The name or ID of the result of the action declaration, such as a test or build artifact.
    outputArtifacts :: Lude.Maybe [OutputArtifact],
    -- | The variable namespace associated with the action. All variables produced as output by this action fall under this namespace.
    namespace :: Lude.Maybe Lude.Text,
    -- | The order in which actions are run.
    runOrder :: Lude.Maybe Lude.Natural,
    -- | The action declaration's name.
    name :: Lude.Text,
    -- | The action declaration's AWS Region, such as us-east-1.
    region :: Lude.Maybe Lude.Text,
    -- | The action's configuration. These are key-value pairs that specify input values for an action. For more information, see <https://docs.aws.amazon.com/codepipeline/latest/userguide/reference-pipeline-structure.html#action-requirements Action Structure Requirements in CodePipeline> . For the list of configuration properties for the AWS CloudFormation action type in CodePipeline, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/continuous-delivery-codepipeline-action-reference.html Configuration Properties Reference> in the /AWS CloudFormation User Guide/ . For template snippets with examples, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/continuous-delivery-codepipeline-parameter-override-functions.html Using Parameter Override Functions with CodePipeline Pipelines> in the /AWS CloudFormation User Guide/ .
    --
    -- The values can be represented in either JSON or YAML format. For example, the JSON configuration item format is as follows:
    -- /JSON:/
    -- @"Configuration" : { Key : Value },@
    configuration :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | Specifies the action type and the provider of the action.
    actionTypeId :: ActionTypeId,
    -- | The name or ID of the artifact consumed by the action, such as a test or build artifact.
    inputArtifacts :: Lude.Maybe [InputArtifact],
    -- | The ARN of the IAM service role that performs the declared action. This is assumed through the roleArn for the pipeline.
    roleARN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ActionDeclaration' with the minimum fields required to make a request.
--
-- * 'outputArtifacts' - The name or ID of the result of the action declaration, such as a test or build artifact.
-- * 'namespace' - The variable namespace associated with the action. All variables produced as output by this action fall under this namespace.
-- * 'runOrder' - The order in which actions are run.
-- * 'name' - The action declaration's name.
-- * 'region' - The action declaration's AWS Region, such as us-east-1.
-- * 'configuration' - The action's configuration. These are key-value pairs that specify input values for an action. For more information, see <https://docs.aws.amazon.com/codepipeline/latest/userguide/reference-pipeline-structure.html#action-requirements Action Structure Requirements in CodePipeline> . For the list of configuration properties for the AWS CloudFormation action type in CodePipeline, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/continuous-delivery-codepipeline-action-reference.html Configuration Properties Reference> in the /AWS CloudFormation User Guide/ . For template snippets with examples, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/continuous-delivery-codepipeline-parameter-override-functions.html Using Parameter Override Functions with CodePipeline Pipelines> in the /AWS CloudFormation User Guide/ .
--
-- The values can be represented in either JSON or YAML format. For example, the JSON configuration item format is as follows:
-- /JSON:/
-- @"Configuration" : { Key : Value },@
-- * 'actionTypeId' - Specifies the action type and the provider of the action.
-- * 'inputArtifacts' - The name or ID of the artifact consumed by the action, such as a test or build artifact.
-- * 'roleARN' - The ARN of the IAM service role that performs the declared action. This is assumed through the roleArn for the pipeline.
mkActionDeclaration ::
  -- | 'name'
  Lude.Text ->
  -- | 'actionTypeId'
  ActionTypeId ->
  ActionDeclaration
mkActionDeclaration pName_ pActionTypeId_ =
  ActionDeclaration'
    { outputArtifacts = Lude.Nothing,
      namespace = Lude.Nothing,
      runOrder = Lude.Nothing,
      name = pName_,
      region = Lude.Nothing,
      configuration = Lude.Nothing,
      actionTypeId = pActionTypeId_,
      inputArtifacts = Lude.Nothing,
      roleARN = Lude.Nothing
    }

-- | The name or ID of the result of the action declaration, such as a test or build artifact.
--
-- /Note:/ Consider using 'outputArtifacts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adOutputArtifacts :: Lens.Lens' ActionDeclaration (Lude.Maybe [OutputArtifact])
adOutputArtifacts = Lens.lens (outputArtifacts :: ActionDeclaration -> Lude.Maybe [OutputArtifact]) (\s a -> s {outputArtifacts = a} :: ActionDeclaration)
{-# DEPRECATED adOutputArtifacts "Use generic-lens or generic-optics with 'outputArtifacts' instead." #-}

-- | The variable namespace associated with the action. All variables produced as output by this action fall under this namespace.
--
-- /Note:/ Consider using 'namespace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adNamespace :: Lens.Lens' ActionDeclaration (Lude.Maybe Lude.Text)
adNamespace = Lens.lens (namespace :: ActionDeclaration -> Lude.Maybe Lude.Text) (\s a -> s {namespace = a} :: ActionDeclaration)
{-# DEPRECATED adNamespace "Use generic-lens or generic-optics with 'namespace' instead." #-}

-- | The order in which actions are run.
--
-- /Note:/ Consider using 'runOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adRunOrder :: Lens.Lens' ActionDeclaration (Lude.Maybe Lude.Natural)
adRunOrder = Lens.lens (runOrder :: ActionDeclaration -> Lude.Maybe Lude.Natural) (\s a -> s {runOrder = a} :: ActionDeclaration)
{-# DEPRECATED adRunOrder "Use generic-lens or generic-optics with 'runOrder' instead." #-}

-- | The action declaration's name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adName :: Lens.Lens' ActionDeclaration Lude.Text
adName = Lens.lens (name :: ActionDeclaration -> Lude.Text) (\s a -> s {name = a} :: ActionDeclaration)
{-# DEPRECATED adName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The action declaration's AWS Region, such as us-east-1.
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adRegion :: Lens.Lens' ActionDeclaration (Lude.Maybe Lude.Text)
adRegion = Lens.lens (region :: ActionDeclaration -> Lude.Maybe Lude.Text) (\s a -> s {region = a} :: ActionDeclaration)
{-# DEPRECATED adRegion "Use generic-lens or generic-optics with 'region' instead." #-}

-- | The action's configuration. These are key-value pairs that specify input values for an action. For more information, see <https://docs.aws.amazon.com/codepipeline/latest/userguide/reference-pipeline-structure.html#action-requirements Action Structure Requirements in CodePipeline> . For the list of configuration properties for the AWS CloudFormation action type in CodePipeline, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/continuous-delivery-codepipeline-action-reference.html Configuration Properties Reference> in the /AWS CloudFormation User Guide/ . For template snippets with examples, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/continuous-delivery-codepipeline-parameter-override-functions.html Using Parameter Override Functions with CodePipeline Pipelines> in the /AWS CloudFormation User Guide/ .
--
-- The values can be represented in either JSON or YAML format. For example, the JSON configuration item format is as follows:
-- /JSON:/
-- @"Configuration" : { Key : Value },@
--
-- /Note:/ Consider using 'configuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adConfiguration :: Lens.Lens' ActionDeclaration (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
adConfiguration = Lens.lens (configuration :: ActionDeclaration -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {configuration = a} :: ActionDeclaration)
{-# DEPRECATED adConfiguration "Use generic-lens or generic-optics with 'configuration' instead." #-}

-- | Specifies the action type and the provider of the action.
--
-- /Note:/ Consider using 'actionTypeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adActionTypeId :: Lens.Lens' ActionDeclaration ActionTypeId
adActionTypeId = Lens.lens (actionTypeId :: ActionDeclaration -> ActionTypeId) (\s a -> s {actionTypeId = a} :: ActionDeclaration)
{-# DEPRECATED adActionTypeId "Use generic-lens or generic-optics with 'actionTypeId' instead." #-}

-- | The name or ID of the artifact consumed by the action, such as a test or build artifact.
--
-- /Note:/ Consider using 'inputArtifacts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adInputArtifacts :: Lens.Lens' ActionDeclaration (Lude.Maybe [InputArtifact])
adInputArtifacts = Lens.lens (inputArtifacts :: ActionDeclaration -> Lude.Maybe [InputArtifact]) (\s a -> s {inputArtifacts = a} :: ActionDeclaration)
{-# DEPRECATED adInputArtifacts "Use generic-lens or generic-optics with 'inputArtifacts' instead." #-}

-- | The ARN of the IAM service role that performs the declared action. This is assumed through the roleArn for the pipeline.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adRoleARN :: Lens.Lens' ActionDeclaration (Lude.Maybe Lude.Text)
adRoleARN = Lens.lens (roleARN :: ActionDeclaration -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: ActionDeclaration)
{-# DEPRECATED adRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.FromJSON ActionDeclaration where
  parseJSON =
    Lude.withObject
      "ActionDeclaration"
      ( \x ->
          ActionDeclaration'
            Lude.<$> (x Lude..:? "outputArtifacts" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "namespace")
            Lude.<*> (x Lude..:? "runOrder")
            Lude.<*> (x Lude..: "name")
            Lude.<*> (x Lude..:? "region")
            Lude.<*> (x Lude..:? "configuration" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..: "actionTypeId")
            Lude.<*> (x Lude..:? "inputArtifacts" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "roleArn")
      )

instance Lude.ToJSON ActionDeclaration where
  toJSON ActionDeclaration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("outputArtifacts" Lude..=) Lude.<$> outputArtifacts,
            ("namespace" Lude..=) Lude.<$> namespace,
            ("runOrder" Lude..=) Lude.<$> runOrder,
            Lude.Just ("name" Lude..= name),
            ("region" Lude..=) Lude.<$> region,
            ("configuration" Lude..=) Lude.<$> configuration,
            Lude.Just ("actionTypeId" Lude..= actionTypeId),
            ("inputArtifacts" Lude..=) Lude.<$> inputArtifacts,
            ("roleArn" Lude..=) Lude.<$> roleARN
          ]
      )

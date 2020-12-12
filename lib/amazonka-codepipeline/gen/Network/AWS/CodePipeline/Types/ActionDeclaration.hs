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
    adRegion,
    adConfiguration,
    adInputArtifacts,
    adRoleARN,
    adName,
    adActionTypeId,
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
  { outputArtifacts ::
      Lude.Maybe [OutputArtifact],
    namespace :: Lude.Maybe Lude.Text,
    runOrder :: Lude.Maybe Lude.Natural,
    region :: Lude.Maybe Lude.Text,
    configuration ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    inputArtifacts :: Lude.Maybe [InputArtifact],
    roleARN :: Lude.Maybe Lude.Text,
    name :: Lude.Text,
    actionTypeId :: ActionTypeId
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ActionDeclaration' with the minimum fields required to make a request.
--
-- * 'actionTypeId' - Specifies the action type and the provider of the action.
-- * 'configuration' - The action's configuration. These are key-value pairs that specify input values for an action. For more information, see <https://docs.aws.amazon.com/codepipeline/latest/userguide/reference-pipeline-structure.html#action-requirements Action Structure Requirements in CodePipeline> . For the list of configuration properties for the AWS CloudFormation action type in CodePipeline, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/continuous-delivery-codepipeline-action-reference.html Configuration Properties Reference> in the /AWS CloudFormation User Guide/ . For template snippets with examples, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/continuous-delivery-codepipeline-parameter-override-functions.html Using Parameter Override Functions with CodePipeline Pipelines> in the /AWS CloudFormation User Guide/ .
--
-- The values can be represented in either JSON or YAML format. For example, the JSON configuration item format is as follows:
-- /JSON:/
-- @"Configuration" : { Key : Value },@
-- * 'inputArtifacts' - The name or ID of the artifact consumed by the action, such as a test or build artifact.
-- * 'name' - The action declaration's name.
-- * 'namespace' - The variable namespace associated with the action. All variables produced as output by this action fall under this namespace.
-- * 'outputArtifacts' - The name or ID of the result of the action declaration, such as a test or build artifact.
-- * 'region' - The action declaration's AWS Region, such as us-east-1.
-- * 'roleARN' - The ARN of the IAM service role that performs the declared action. This is assumed through the roleArn for the pipeline.
-- * 'runOrder' - The order in which actions are run.
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
      region = Lude.Nothing,
      configuration = Lude.Nothing,
      inputArtifacts = Lude.Nothing,
      roleARN = Lude.Nothing,
      name = pName_,
      actionTypeId = pActionTypeId_
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

-- | The action declaration's name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adName :: Lens.Lens' ActionDeclaration Lude.Text
adName = Lens.lens (name :: ActionDeclaration -> Lude.Text) (\s a -> s {name = a} :: ActionDeclaration)
{-# DEPRECATED adName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Specifies the action type and the provider of the action.
--
-- /Note:/ Consider using 'actionTypeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adActionTypeId :: Lens.Lens' ActionDeclaration ActionTypeId
adActionTypeId = Lens.lens (actionTypeId :: ActionDeclaration -> ActionTypeId) (\s a -> s {actionTypeId = a} :: ActionDeclaration)
{-# DEPRECATED adActionTypeId "Use generic-lens or generic-optics with 'actionTypeId' instead." #-}

instance Lude.FromJSON ActionDeclaration where
  parseJSON =
    Lude.withObject
      "ActionDeclaration"
      ( \x ->
          ActionDeclaration'
            Lude.<$> (x Lude..:? "outputArtifacts" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "namespace")
            Lude.<*> (x Lude..:? "runOrder")
            Lude.<*> (x Lude..:? "region")
            Lude.<*> (x Lude..:? "configuration" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "inputArtifacts" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "roleArn")
            Lude.<*> (x Lude..: "name")
            Lude.<*> (x Lude..: "actionTypeId")
      )

instance Lude.ToJSON ActionDeclaration where
  toJSON ActionDeclaration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("outputArtifacts" Lude..=) Lude.<$> outputArtifacts,
            ("namespace" Lude..=) Lude.<$> namespace,
            ("runOrder" Lude..=) Lude.<$> runOrder,
            ("region" Lude..=) Lude.<$> region,
            ("configuration" Lude..=) Lude.<$> configuration,
            ("inputArtifacts" Lude..=) Lude.<$> inputArtifacts,
            ("roleArn" Lude..=) Lude.<$> roleARN,
            Lude.Just ("name" Lude..= name),
            Lude.Just ("actionTypeId" Lude..= actionTypeId)
          ]
      )

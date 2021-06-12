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
-- Module      : Network.AWS.CodePipeline.Types.ActionDeclaration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.ActionDeclaration where

import Network.AWS.CodePipeline.Types.ActionTypeId
import Network.AWS.CodePipeline.Types.InputArtifact
import Network.AWS.CodePipeline.Types.OutputArtifact
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Represents information about an action declaration.
--
-- /See:/ 'newActionDeclaration' smart constructor.
data ActionDeclaration = ActionDeclaration'
  { -- | The ARN of the IAM service role that performs the declared action. This
    -- is assumed through the roleArn for the pipeline.
    roleArn :: Core.Maybe Core.Text,
    -- | The action\'s configuration. These are key-value pairs that specify
    -- input values for an action. For more information, see
    -- <https://docs.aws.amazon.com/codepipeline/latest/userguide/reference-pipeline-structure.html#action-requirements Action Structure Requirements in CodePipeline>.
    -- For the list of configuration properties for the AWS CloudFormation
    -- action type in CodePipeline, see
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/continuous-delivery-codepipeline-action-reference.html Configuration Properties Reference>
    -- in the /AWS CloudFormation User Guide/. For template snippets with
    -- examples, see
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/continuous-delivery-codepipeline-parameter-override-functions.html Using Parameter Override Functions with CodePipeline Pipelines>
    -- in the /AWS CloudFormation User Guide/.
    --
    -- The values can be represented in either JSON or YAML format. For
    -- example, the JSON configuration item format is as follows:
    --
    -- /JSON:/
    --
    -- @\"Configuration\" : { Key : Value },@
    configuration :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The order in which actions are run.
    runOrder :: Core.Maybe Core.Natural,
    -- | The variable namespace associated with the action. All variables
    -- produced as output by this action fall under this namespace.
    namespace :: Core.Maybe Core.Text,
    -- | The name or ID of the artifact consumed by the action, such as a test or
    -- build artifact.
    inputArtifacts :: Core.Maybe [InputArtifact],
    -- | The action declaration\'s AWS Region, such as us-east-1.
    region :: Core.Maybe Core.Text,
    -- | The name or ID of the result of the action declaration, such as a test
    -- or build artifact.
    outputArtifacts :: Core.Maybe [OutputArtifact],
    -- | The action declaration\'s name.
    name :: Core.Text,
    -- | Specifies the action type and the provider of the action.
    actionTypeId :: ActionTypeId
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ActionDeclaration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleArn', 'actionDeclaration_roleArn' - The ARN of the IAM service role that performs the declared action. This
-- is assumed through the roleArn for the pipeline.
--
-- 'configuration', 'actionDeclaration_configuration' - The action\'s configuration. These are key-value pairs that specify
-- input values for an action. For more information, see
-- <https://docs.aws.amazon.com/codepipeline/latest/userguide/reference-pipeline-structure.html#action-requirements Action Structure Requirements in CodePipeline>.
-- For the list of configuration properties for the AWS CloudFormation
-- action type in CodePipeline, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/continuous-delivery-codepipeline-action-reference.html Configuration Properties Reference>
-- in the /AWS CloudFormation User Guide/. For template snippets with
-- examples, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/continuous-delivery-codepipeline-parameter-override-functions.html Using Parameter Override Functions with CodePipeline Pipelines>
-- in the /AWS CloudFormation User Guide/.
--
-- The values can be represented in either JSON or YAML format. For
-- example, the JSON configuration item format is as follows:
--
-- /JSON:/
--
-- @\"Configuration\" : { Key : Value },@
--
-- 'runOrder', 'actionDeclaration_runOrder' - The order in which actions are run.
--
-- 'namespace', 'actionDeclaration_namespace' - The variable namespace associated with the action. All variables
-- produced as output by this action fall under this namespace.
--
-- 'inputArtifacts', 'actionDeclaration_inputArtifacts' - The name or ID of the artifact consumed by the action, such as a test or
-- build artifact.
--
-- 'region', 'actionDeclaration_region' - The action declaration\'s AWS Region, such as us-east-1.
--
-- 'outputArtifacts', 'actionDeclaration_outputArtifacts' - The name or ID of the result of the action declaration, such as a test
-- or build artifact.
--
-- 'name', 'actionDeclaration_name' - The action declaration\'s name.
--
-- 'actionTypeId', 'actionDeclaration_actionTypeId' - Specifies the action type and the provider of the action.
newActionDeclaration ::
  -- | 'name'
  Core.Text ->
  -- | 'actionTypeId'
  ActionTypeId ->
  ActionDeclaration
newActionDeclaration pName_ pActionTypeId_ =
  ActionDeclaration'
    { roleArn = Core.Nothing,
      configuration = Core.Nothing,
      runOrder = Core.Nothing,
      namespace = Core.Nothing,
      inputArtifacts = Core.Nothing,
      region = Core.Nothing,
      outputArtifacts = Core.Nothing,
      name = pName_,
      actionTypeId = pActionTypeId_
    }

-- | The ARN of the IAM service role that performs the declared action. This
-- is assumed through the roleArn for the pipeline.
actionDeclaration_roleArn :: Lens.Lens' ActionDeclaration (Core.Maybe Core.Text)
actionDeclaration_roleArn = Lens.lens (\ActionDeclaration' {roleArn} -> roleArn) (\s@ActionDeclaration' {} a -> s {roleArn = a} :: ActionDeclaration)

-- | The action\'s configuration. These are key-value pairs that specify
-- input values for an action. For more information, see
-- <https://docs.aws.amazon.com/codepipeline/latest/userguide/reference-pipeline-structure.html#action-requirements Action Structure Requirements in CodePipeline>.
-- For the list of configuration properties for the AWS CloudFormation
-- action type in CodePipeline, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/continuous-delivery-codepipeline-action-reference.html Configuration Properties Reference>
-- in the /AWS CloudFormation User Guide/. For template snippets with
-- examples, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/continuous-delivery-codepipeline-parameter-override-functions.html Using Parameter Override Functions with CodePipeline Pipelines>
-- in the /AWS CloudFormation User Guide/.
--
-- The values can be represented in either JSON or YAML format. For
-- example, the JSON configuration item format is as follows:
--
-- /JSON:/
--
-- @\"Configuration\" : { Key : Value },@
actionDeclaration_configuration :: Lens.Lens' ActionDeclaration (Core.Maybe (Core.HashMap Core.Text Core.Text))
actionDeclaration_configuration = Lens.lens (\ActionDeclaration' {configuration} -> configuration) (\s@ActionDeclaration' {} a -> s {configuration = a} :: ActionDeclaration) Core.. Lens.mapping Lens._Coerce

-- | The order in which actions are run.
actionDeclaration_runOrder :: Lens.Lens' ActionDeclaration (Core.Maybe Core.Natural)
actionDeclaration_runOrder = Lens.lens (\ActionDeclaration' {runOrder} -> runOrder) (\s@ActionDeclaration' {} a -> s {runOrder = a} :: ActionDeclaration)

-- | The variable namespace associated with the action. All variables
-- produced as output by this action fall under this namespace.
actionDeclaration_namespace :: Lens.Lens' ActionDeclaration (Core.Maybe Core.Text)
actionDeclaration_namespace = Lens.lens (\ActionDeclaration' {namespace} -> namespace) (\s@ActionDeclaration' {} a -> s {namespace = a} :: ActionDeclaration)

-- | The name or ID of the artifact consumed by the action, such as a test or
-- build artifact.
actionDeclaration_inputArtifacts :: Lens.Lens' ActionDeclaration (Core.Maybe [InputArtifact])
actionDeclaration_inputArtifacts = Lens.lens (\ActionDeclaration' {inputArtifacts} -> inputArtifacts) (\s@ActionDeclaration' {} a -> s {inputArtifacts = a} :: ActionDeclaration) Core.. Lens.mapping Lens._Coerce

-- | The action declaration\'s AWS Region, such as us-east-1.
actionDeclaration_region :: Lens.Lens' ActionDeclaration (Core.Maybe Core.Text)
actionDeclaration_region = Lens.lens (\ActionDeclaration' {region} -> region) (\s@ActionDeclaration' {} a -> s {region = a} :: ActionDeclaration)

-- | The name or ID of the result of the action declaration, such as a test
-- or build artifact.
actionDeclaration_outputArtifacts :: Lens.Lens' ActionDeclaration (Core.Maybe [OutputArtifact])
actionDeclaration_outputArtifacts = Lens.lens (\ActionDeclaration' {outputArtifacts} -> outputArtifacts) (\s@ActionDeclaration' {} a -> s {outputArtifacts = a} :: ActionDeclaration) Core.. Lens.mapping Lens._Coerce

-- | The action declaration\'s name.
actionDeclaration_name :: Lens.Lens' ActionDeclaration Core.Text
actionDeclaration_name = Lens.lens (\ActionDeclaration' {name} -> name) (\s@ActionDeclaration' {} a -> s {name = a} :: ActionDeclaration)

-- | Specifies the action type and the provider of the action.
actionDeclaration_actionTypeId :: Lens.Lens' ActionDeclaration ActionTypeId
actionDeclaration_actionTypeId = Lens.lens (\ActionDeclaration' {actionTypeId} -> actionTypeId) (\s@ActionDeclaration' {} a -> s {actionTypeId = a} :: ActionDeclaration)

instance Core.FromJSON ActionDeclaration where
  parseJSON =
    Core.withObject
      "ActionDeclaration"
      ( \x ->
          ActionDeclaration'
            Core.<$> (x Core..:? "roleArn")
            Core.<*> (x Core..:? "configuration" Core..!= Core.mempty)
            Core.<*> (x Core..:? "runOrder")
            Core.<*> (x Core..:? "namespace")
            Core.<*> (x Core..:? "inputArtifacts" Core..!= Core.mempty)
            Core.<*> (x Core..:? "region")
            Core.<*> (x Core..:? "outputArtifacts" Core..!= Core.mempty)
            Core.<*> (x Core..: "name")
            Core.<*> (x Core..: "actionTypeId")
      )

instance Core.Hashable ActionDeclaration

instance Core.NFData ActionDeclaration

instance Core.ToJSON ActionDeclaration where
  toJSON ActionDeclaration' {..} =
    Core.object
      ( Core.catMaybes
          [ ("roleArn" Core..=) Core.<$> roleArn,
            ("configuration" Core..=) Core.<$> configuration,
            ("runOrder" Core..=) Core.<$> runOrder,
            ("namespace" Core..=) Core.<$> namespace,
            ("inputArtifacts" Core..=) Core.<$> inputArtifacts,
            ("region" Core..=) Core.<$> region,
            ("outputArtifacts" Core..=) Core.<$> outputArtifacts,
            Core.Just ("name" Core..= name),
            Core.Just ("actionTypeId" Core..= actionTypeId)
          ]
      )

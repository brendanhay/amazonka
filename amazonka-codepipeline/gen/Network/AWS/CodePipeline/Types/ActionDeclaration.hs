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
import qualified Network.AWS.Prelude as Prelude

-- | Represents information about an action declaration.
--
-- /See:/ 'newActionDeclaration' smart constructor.
data ActionDeclaration = ActionDeclaration'
  { -- | The ARN of the IAM service role that performs the declared action. This
    -- is assumed through the roleArn for the pipeline.
    roleArn :: Prelude.Maybe Prelude.Text,
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
    configuration :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The order in which actions are run.
    runOrder :: Prelude.Maybe Prelude.Natural,
    -- | The name or ID of the artifact consumed by the action, such as a test or
    -- build artifact.
    inputArtifacts :: Prelude.Maybe [InputArtifact],
    -- | The variable namespace associated with the action. All variables
    -- produced as output by this action fall under this namespace.
    namespace :: Prelude.Maybe Prelude.Text,
    -- | The action declaration\'s AWS Region, such as us-east-1.
    region :: Prelude.Maybe Prelude.Text,
    -- | The name or ID of the result of the action declaration, such as a test
    -- or build artifact.
    outputArtifacts :: Prelude.Maybe [OutputArtifact],
    -- | The action declaration\'s name.
    name :: Prelude.Text,
    -- | Specifies the action type and the provider of the action.
    actionTypeId :: ActionTypeId
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'inputArtifacts', 'actionDeclaration_inputArtifacts' - The name or ID of the artifact consumed by the action, such as a test or
-- build artifact.
--
-- 'namespace', 'actionDeclaration_namespace' - The variable namespace associated with the action. All variables
-- produced as output by this action fall under this namespace.
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
  Prelude.Text ->
  -- | 'actionTypeId'
  ActionTypeId ->
  ActionDeclaration
newActionDeclaration pName_ pActionTypeId_ =
  ActionDeclaration'
    { roleArn = Prelude.Nothing,
      configuration = Prelude.Nothing,
      runOrder = Prelude.Nothing,
      inputArtifacts = Prelude.Nothing,
      namespace = Prelude.Nothing,
      region = Prelude.Nothing,
      outputArtifacts = Prelude.Nothing,
      name = pName_,
      actionTypeId = pActionTypeId_
    }

-- | The ARN of the IAM service role that performs the declared action. This
-- is assumed through the roleArn for the pipeline.
actionDeclaration_roleArn :: Lens.Lens' ActionDeclaration (Prelude.Maybe Prelude.Text)
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
actionDeclaration_configuration :: Lens.Lens' ActionDeclaration (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
actionDeclaration_configuration = Lens.lens (\ActionDeclaration' {configuration} -> configuration) (\s@ActionDeclaration' {} a -> s {configuration = a} :: ActionDeclaration) Prelude.. Lens.mapping Lens._Coerce

-- | The order in which actions are run.
actionDeclaration_runOrder :: Lens.Lens' ActionDeclaration (Prelude.Maybe Prelude.Natural)
actionDeclaration_runOrder = Lens.lens (\ActionDeclaration' {runOrder} -> runOrder) (\s@ActionDeclaration' {} a -> s {runOrder = a} :: ActionDeclaration)

-- | The name or ID of the artifact consumed by the action, such as a test or
-- build artifact.
actionDeclaration_inputArtifacts :: Lens.Lens' ActionDeclaration (Prelude.Maybe [InputArtifact])
actionDeclaration_inputArtifacts = Lens.lens (\ActionDeclaration' {inputArtifacts} -> inputArtifacts) (\s@ActionDeclaration' {} a -> s {inputArtifacts = a} :: ActionDeclaration) Prelude.. Lens.mapping Lens._Coerce

-- | The variable namespace associated with the action. All variables
-- produced as output by this action fall under this namespace.
actionDeclaration_namespace :: Lens.Lens' ActionDeclaration (Prelude.Maybe Prelude.Text)
actionDeclaration_namespace = Lens.lens (\ActionDeclaration' {namespace} -> namespace) (\s@ActionDeclaration' {} a -> s {namespace = a} :: ActionDeclaration)

-- | The action declaration\'s AWS Region, such as us-east-1.
actionDeclaration_region :: Lens.Lens' ActionDeclaration (Prelude.Maybe Prelude.Text)
actionDeclaration_region = Lens.lens (\ActionDeclaration' {region} -> region) (\s@ActionDeclaration' {} a -> s {region = a} :: ActionDeclaration)

-- | The name or ID of the result of the action declaration, such as a test
-- or build artifact.
actionDeclaration_outputArtifacts :: Lens.Lens' ActionDeclaration (Prelude.Maybe [OutputArtifact])
actionDeclaration_outputArtifacts = Lens.lens (\ActionDeclaration' {outputArtifacts} -> outputArtifacts) (\s@ActionDeclaration' {} a -> s {outputArtifacts = a} :: ActionDeclaration) Prelude.. Lens.mapping Lens._Coerce

-- | The action declaration\'s name.
actionDeclaration_name :: Lens.Lens' ActionDeclaration Prelude.Text
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
            Prelude.<$> (x Core..:? "roleArn")
            Prelude.<*> (x Core..:? "configuration" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "runOrder")
            Prelude.<*> (x Core..:? "inputArtifacts" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "namespace")
            Prelude.<*> (x Core..:? "region")
            Prelude.<*> ( x Core..:? "outputArtifacts"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..: "name")
            Prelude.<*> (x Core..: "actionTypeId")
      )

instance Prelude.Hashable ActionDeclaration

instance Prelude.NFData ActionDeclaration

instance Core.ToJSON ActionDeclaration where
  toJSON ActionDeclaration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("roleArn" Core..=) Prelude.<$> roleArn,
            ("configuration" Core..=) Prelude.<$> configuration,
            ("runOrder" Core..=) Prelude.<$> runOrder,
            ("inputArtifacts" Core..=)
              Prelude.<$> inputArtifacts,
            ("namespace" Core..=) Prelude.<$> namespace,
            ("region" Core..=) Prelude.<$> region,
            ("outputArtifacts" Core..=)
              Prelude.<$> outputArtifacts,
            Prelude.Just ("name" Core..= name),
            Prelude.Just ("actionTypeId" Core..= actionTypeId)
          ]
      )

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
-- Module      : Amazonka.CodePipeline.Types.ActionDeclaration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodePipeline.Types.ActionDeclaration where

import Amazonka.CodePipeline.Types.ActionTypeId
import Amazonka.CodePipeline.Types.InputArtifact
import Amazonka.CodePipeline.Types.OutputArtifact
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents information about an action declaration.
--
-- /See:/ 'newActionDeclaration' smart constructor.
data ActionDeclaration = ActionDeclaration'
  { -- | The ARN of the IAM service role that performs the declared action. This
    -- is assumed through the roleArn for the pipeline.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The order in which actions are run.
    runOrder :: Prelude.Maybe Prelude.Natural,
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
    -- | The name or ID of the result of the action declaration, such as a test
    -- or build artifact.
    outputArtifacts :: Prelude.Maybe [OutputArtifact],
    -- | The action declaration\'s AWS Region, such as us-east-1.
    region :: Prelude.Maybe Prelude.Text,
    -- | The name or ID of the artifact consumed by the action, such as a test or
    -- build artifact.
    inputArtifacts :: Prelude.Maybe [InputArtifact],
    -- | The variable namespace associated with the action. All variables
    -- produced as output by this action fall under this namespace.
    namespace :: Prelude.Maybe Prelude.Text,
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
-- 'runOrder', 'actionDeclaration_runOrder' - The order in which actions are run.
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
-- 'outputArtifacts', 'actionDeclaration_outputArtifacts' - The name or ID of the result of the action declaration, such as a test
-- or build artifact.
--
-- 'region', 'actionDeclaration_region' - The action declaration\'s AWS Region, such as us-east-1.
--
-- 'inputArtifacts', 'actionDeclaration_inputArtifacts' - The name or ID of the artifact consumed by the action, such as a test or
-- build artifact.
--
-- 'namespace', 'actionDeclaration_namespace' - The variable namespace associated with the action. All variables
-- produced as output by this action fall under this namespace.
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
      runOrder = Prelude.Nothing,
      configuration = Prelude.Nothing,
      outputArtifacts = Prelude.Nothing,
      region = Prelude.Nothing,
      inputArtifacts = Prelude.Nothing,
      namespace = Prelude.Nothing,
      name = pName_,
      actionTypeId = pActionTypeId_
    }

-- | The ARN of the IAM service role that performs the declared action. This
-- is assumed through the roleArn for the pipeline.
actionDeclaration_roleArn :: Lens.Lens' ActionDeclaration (Prelude.Maybe Prelude.Text)
actionDeclaration_roleArn = Lens.lens (\ActionDeclaration' {roleArn} -> roleArn) (\s@ActionDeclaration' {} a -> s {roleArn = a} :: ActionDeclaration)

-- | The order in which actions are run.
actionDeclaration_runOrder :: Lens.Lens' ActionDeclaration (Prelude.Maybe Prelude.Natural)
actionDeclaration_runOrder = Lens.lens (\ActionDeclaration' {runOrder} -> runOrder) (\s@ActionDeclaration' {} a -> s {runOrder = a} :: ActionDeclaration)

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
actionDeclaration_configuration = Lens.lens (\ActionDeclaration' {configuration} -> configuration) (\s@ActionDeclaration' {} a -> s {configuration = a} :: ActionDeclaration) Prelude.. Lens.mapping Lens.coerced

-- | The name or ID of the result of the action declaration, such as a test
-- or build artifact.
actionDeclaration_outputArtifacts :: Lens.Lens' ActionDeclaration (Prelude.Maybe [OutputArtifact])
actionDeclaration_outputArtifacts = Lens.lens (\ActionDeclaration' {outputArtifacts} -> outputArtifacts) (\s@ActionDeclaration' {} a -> s {outputArtifacts = a} :: ActionDeclaration) Prelude.. Lens.mapping Lens.coerced

-- | The action declaration\'s AWS Region, such as us-east-1.
actionDeclaration_region :: Lens.Lens' ActionDeclaration (Prelude.Maybe Prelude.Text)
actionDeclaration_region = Lens.lens (\ActionDeclaration' {region} -> region) (\s@ActionDeclaration' {} a -> s {region = a} :: ActionDeclaration)

-- | The name or ID of the artifact consumed by the action, such as a test or
-- build artifact.
actionDeclaration_inputArtifacts :: Lens.Lens' ActionDeclaration (Prelude.Maybe [InputArtifact])
actionDeclaration_inputArtifacts = Lens.lens (\ActionDeclaration' {inputArtifacts} -> inputArtifacts) (\s@ActionDeclaration' {} a -> s {inputArtifacts = a} :: ActionDeclaration) Prelude.. Lens.mapping Lens.coerced

-- | The variable namespace associated with the action. All variables
-- produced as output by this action fall under this namespace.
actionDeclaration_namespace :: Lens.Lens' ActionDeclaration (Prelude.Maybe Prelude.Text)
actionDeclaration_namespace = Lens.lens (\ActionDeclaration' {namespace} -> namespace) (\s@ActionDeclaration' {} a -> s {namespace = a} :: ActionDeclaration)

-- | The action declaration\'s name.
actionDeclaration_name :: Lens.Lens' ActionDeclaration Prelude.Text
actionDeclaration_name = Lens.lens (\ActionDeclaration' {name} -> name) (\s@ActionDeclaration' {} a -> s {name = a} :: ActionDeclaration)

-- | Specifies the action type and the provider of the action.
actionDeclaration_actionTypeId :: Lens.Lens' ActionDeclaration ActionTypeId
actionDeclaration_actionTypeId = Lens.lens (\ActionDeclaration' {actionTypeId} -> actionTypeId) (\s@ActionDeclaration' {} a -> s {actionTypeId = a} :: ActionDeclaration)

instance Data.FromJSON ActionDeclaration where
  parseJSON =
    Data.withObject
      "ActionDeclaration"
      ( \x ->
          ActionDeclaration'
            Prelude.<$> (x Data..:? "roleArn")
            Prelude.<*> (x Data..:? "runOrder")
            Prelude.<*> (x Data..:? "configuration" Data..!= Prelude.mempty)
            Prelude.<*> ( x Data..:? "outputArtifacts"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "region")
            Prelude.<*> (x Data..:? "inputArtifacts" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "namespace")
            Prelude.<*> (x Data..: "name")
            Prelude.<*> (x Data..: "actionTypeId")
      )

instance Prelude.Hashable ActionDeclaration where
  hashWithSalt _salt ActionDeclaration' {..} =
    _salt `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` runOrder
      `Prelude.hashWithSalt` configuration
      `Prelude.hashWithSalt` outputArtifacts
      `Prelude.hashWithSalt` region
      `Prelude.hashWithSalt` inputArtifacts
      `Prelude.hashWithSalt` namespace
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` actionTypeId

instance Prelude.NFData ActionDeclaration where
  rnf ActionDeclaration' {..} =
    Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf runOrder
      `Prelude.seq` Prelude.rnf configuration
      `Prelude.seq` Prelude.rnf outputArtifacts
      `Prelude.seq` Prelude.rnf region
      `Prelude.seq` Prelude.rnf inputArtifacts
      `Prelude.seq` Prelude.rnf namespace
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf actionTypeId

instance Data.ToJSON ActionDeclaration where
  toJSON ActionDeclaration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("roleArn" Data..=) Prelude.<$> roleArn,
            ("runOrder" Data..=) Prelude.<$> runOrder,
            ("configuration" Data..=) Prelude.<$> configuration,
            ("outputArtifacts" Data..=)
              Prelude.<$> outputArtifacts,
            ("region" Data..=) Prelude.<$> region,
            ("inputArtifacts" Data..=)
              Prelude.<$> inputArtifacts,
            ("namespace" Data..=) Prelude.<$> namespace,
            Prelude.Just ("name" Data..= name),
            Prelude.Just ("actionTypeId" Data..= actionTypeId)
          ]
      )

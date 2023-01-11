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
-- Module      : Amazonka.CodePipeline.Types.ActionExecutionInput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodePipeline.Types.ActionExecutionInput where

import Amazonka.CodePipeline.Types.ActionTypeId
import Amazonka.CodePipeline.Types.ArtifactDetail
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Input information used for an action execution.
--
-- /See:/ 'newActionExecutionInput' smart constructor.
data ActionExecutionInput = ActionExecutionInput'
  { actionTypeId :: Prelude.Maybe ActionTypeId,
    -- | Configuration data for an action execution.
    configuration :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Details of input artifacts of the action that correspond to the action
    -- execution.
    inputArtifacts :: Prelude.Maybe [ArtifactDetail],
    -- | The variable namespace associated with the action. All variables
    -- produced as output by this action fall under this namespace.
    namespace :: Prelude.Maybe Prelude.Text,
    -- | The AWS Region for the action, such as us-east-1.
    region :: Prelude.Maybe Prelude.Text,
    -- | Configuration data for an action execution with all variable references
    -- replaced with their real values for the execution.
    resolvedConfiguration :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The ARN of the IAM service role that performs the declared action. This
    -- is assumed through the roleArn for the pipeline.
    roleArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ActionExecutionInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actionTypeId', 'actionExecutionInput_actionTypeId' - Undocumented member.
--
-- 'configuration', 'actionExecutionInput_configuration' - Configuration data for an action execution.
--
-- 'inputArtifacts', 'actionExecutionInput_inputArtifacts' - Details of input artifacts of the action that correspond to the action
-- execution.
--
-- 'namespace', 'actionExecutionInput_namespace' - The variable namespace associated with the action. All variables
-- produced as output by this action fall under this namespace.
--
-- 'region', 'actionExecutionInput_region' - The AWS Region for the action, such as us-east-1.
--
-- 'resolvedConfiguration', 'actionExecutionInput_resolvedConfiguration' - Configuration data for an action execution with all variable references
-- replaced with their real values for the execution.
--
-- 'roleArn', 'actionExecutionInput_roleArn' - The ARN of the IAM service role that performs the declared action. This
-- is assumed through the roleArn for the pipeline.
newActionExecutionInput ::
  ActionExecutionInput
newActionExecutionInput =
  ActionExecutionInput'
    { actionTypeId =
        Prelude.Nothing,
      configuration = Prelude.Nothing,
      inputArtifacts = Prelude.Nothing,
      namespace = Prelude.Nothing,
      region = Prelude.Nothing,
      resolvedConfiguration = Prelude.Nothing,
      roleArn = Prelude.Nothing
    }

-- | Undocumented member.
actionExecutionInput_actionTypeId :: Lens.Lens' ActionExecutionInput (Prelude.Maybe ActionTypeId)
actionExecutionInput_actionTypeId = Lens.lens (\ActionExecutionInput' {actionTypeId} -> actionTypeId) (\s@ActionExecutionInput' {} a -> s {actionTypeId = a} :: ActionExecutionInput)

-- | Configuration data for an action execution.
actionExecutionInput_configuration :: Lens.Lens' ActionExecutionInput (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
actionExecutionInput_configuration = Lens.lens (\ActionExecutionInput' {configuration} -> configuration) (\s@ActionExecutionInput' {} a -> s {configuration = a} :: ActionExecutionInput) Prelude.. Lens.mapping Lens.coerced

-- | Details of input artifacts of the action that correspond to the action
-- execution.
actionExecutionInput_inputArtifacts :: Lens.Lens' ActionExecutionInput (Prelude.Maybe [ArtifactDetail])
actionExecutionInput_inputArtifacts = Lens.lens (\ActionExecutionInput' {inputArtifacts} -> inputArtifacts) (\s@ActionExecutionInput' {} a -> s {inputArtifacts = a} :: ActionExecutionInput) Prelude.. Lens.mapping Lens.coerced

-- | The variable namespace associated with the action. All variables
-- produced as output by this action fall under this namespace.
actionExecutionInput_namespace :: Lens.Lens' ActionExecutionInput (Prelude.Maybe Prelude.Text)
actionExecutionInput_namespace = Lens.lens (\ActionExecutionInput' {namespace} -> namespace) (\s@ActionExecutionInput' {} a -> s {namespace = a} :: ActionExecutionInput)

-- | The AWS Region for the action, such as us-east-1.
actionExecutionInput_region :: Lens.Lens' ActionExecutionInput (Prelude.Maybe Prelude.Text)
actionExecutionInput_region = Lens.lens (\ActionExecutionInput' {region} -> region) (\s@ActionExecutionInput' {} a -> s {region = a} :: ActionExecutionInput)

-- | Configuration data for an action execution with all variable references
-- replaced with their real values for the execution.
actionExecutionInput_resolvedConfiguration :: Lens.Lens' ActionExecutionInput (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
actionExecutionInput_resolvedConfiguration = Lens.lens (\ActionExecutionInput' {resolvedConfiguration} -> resolvedConfiguration) (\s@ActionExecutionInput' {} a -> s {resolvedConfiguration = a} :: ActionExecutionInput) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of the IAM service role that performs the declared action. This
-- is assumed through the roleArn for the pipeline.
actionExecutionInput_roleArn :: Lens.Lens' ActionExecutionInput (Prelude.Maybe Prelude.Text)
actionExecutionInput_roleArn = Lens.lens (\ActionExecutionInput' {roleArn} -> roleArn) (\s@ActionExecutionInput' {} a -> s {roleArn = a} :: ActionExecutionInput)

instance Data.FromJSON ActionExecutionInput where
  parseJSON =
    Data.withObject
      "ActionExecutionInput"
      ( \x ->
          ActionExecutionInput'
            Prelude.<$> (x Data..:? "actionTypeId")
            Prelude.<*> (x Data..:? "configuration" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "inputArtifacts" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "namespace")
            Prelude.<*> (x Data..:? "region")
            Prelude.<*> ( x Data..:? "resolvedConfiguration"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "roleArn")
      )

instance Prelude.Hashable ActionExecutionInput where
  hashWithSalt _salt ActionExecutionInput' {..} =
    _salt `Prelude.hashWithSalt` actionTypeId
      `Prelude.hashWithSalt` configuration
      `Prelude.hashWithSalt` inputArtifacts
      `Prelude.hashWithSalt` namespace
      `Prelude.hashWithSalt` region
      `Prelude.hashWithSalt` resolvedConfiguration
      `Prelude.hashWithSalt` roleArn

instance Prelude.NFData ActionExecutionInput where
  rnf ActionExecutionInput' {..} =
    Prelude.rnf actionTypeId
      `Prelude.seq` Prelude.rnf configuration
      `Prelude.seq` Prelude.rnf inputArtifacts
      `Prelude.seq` Prelude.rnf namespace
      `Prelude.seq` Prelude.rnf region
      `Prelude.seq` Prelude.rnf resolvedConfiguration
      `Prelude.seq` Prelude.rnf roleArn

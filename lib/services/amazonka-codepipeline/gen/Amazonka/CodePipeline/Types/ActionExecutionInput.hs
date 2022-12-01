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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodePipeline.Types.ActionExecutionInput where

import Amazonka.CodePipeline.Types.ActionTypeId
import Amazonka.CodePipeline.Types.ArtifactDetail
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Input information used for an action execution.
--
-- /See:/ 'newActionExecutionInput' smart constructor.
data ActionExecutionInput = ActionExecutionInput'
  { -- | Configuration data for an action execution with all variable references
    -- replaced with their real values for the execution.
    resolvedConfiguration :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The ARN of the IAM service role that performs the declared action. This
    -- is assumed through the roleArn for the pipeline.
    roleArn :: Prelude.Maybe Prelude.Text,
    actionTypeId :: Prelude.Maybe ActionTypeId,
    -- | Configuration data for an action execution.
    configuration :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The AWS Region for the action, such as us-east-1.
    region :: Prelude.Maybe Prelude.Text,
    -- | Details of input artifacts of the action that correspond to the action
    -- execution.
    inputArtifacts :: Prelude.Maybe [ArtifactDetail],
    -- | The variable namespace associated with the action. All variables
    -- produced as output by this action fall under this namespace.
    namespace :: Prelude.Maybe Prelude.Text
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
-- 'resolvedConfiguration', 'actionExecutionInput_resolvedConfiguration' - Configuration data for an action execution with all variable references
-- replaced with their real values for the execution.
--
-- 'roleArn', 'actionExecutionInput_roleArn' - The ARN of the IAM service role that performs the declared action. This
-- is assumed through the roleArn for the pipeline.
--
-- 'actionTypeId', 'actionExecutionInput_actionTypeId' - Undocumented member.
--
-- 'configuration', 'actionExecutionInput_configuration' - Configuration data for an action execution.
--
-- 'region', 'actionExecutionInput_region' - The AWS Region for the action, such as us-east-1.
--
-- 'inputArtifacts', 'actionExecutionInput_inputArtifacts' - Details of input artifacts of the action that correspond to the action
-- execution.
--
-- 'namespace', 'actionExecutionInput_namespace' - The variable namespace associated with the action. All variables
-- produced as output by this action fall under this namespace.
newActionExecutionInput ::
  ActionExecutionInput
newActionExecutionInput =
  ActionExecutionInput'
    { resolvedConfiguration =
        Prelude.Nothing,
      roleArn = Prelude.Nothing,
      actionTypeId = Prelude.Nothing,
      configuration = Prelude.Nothing,
      region = Prelude.Nothing,
      inputArtifacts = Prelude.Nothing,
      namespace = Prelude.Nothing
    }

-- | Configuration data for an action execution with all variable references
-- replaced with their real values for the execution.
actionExecutionInput_resolvedConfiguration :: Lens.Lens' ActionExecutionInput (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
actionExecutionInput_resolvedConfiguration = Lens.lens (\ActionExecutionInput' {resolvedConfiguration} -> resolvedConfiguration) (\s@ActionExecutionInput' {} a -> s {resolvedConfiguration = a} :: ActionExecutionInput) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of the IAM service role that performs the declared action. This
-- is assumed through the roleArn for the pipeline.
actionExecutionInput_roleArn :: Lens.Lens' ActionExecutionInput (Prelude.Maybe Prelude.Text)
actionExecutionInput_roleArn = Lens.lens (\ActionExecutionInput' {roleArn} -> roleArn) (\s@ActionExecutionInput' {} a -> s {roleArn = a} :: ActionExecutionInput)

-- | Undocumented member.
actionExecutionInput_actionTypeId :: Lens.Lens' ActionExecutionInput (Prelude.Maybe ActionTypeId)
actionExecutionInput_actionTypeId = Lens.lens (\ActionExecutionInput' {actionTypeId} -> actionTypeId) (\s@ActionExecutionInput' {} a -> s {actionTypeId = a} :: ActionExecutionInput)

-- | Configuration data for an action execution.
actionExecutionInput_configuration :: Lens.Lens' ActionExecutionInput (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
actionExecutionInput_configuration = Lens.lens (\ActionExecutionInput' {configuration} -> configuration) (\s@ActionExecutionInput' {} a -> s {configuration = a} :: ActionExecutionInput) Prelude.. Lens.mapping Lens.coerced

-- | The AWS Region for the action, such as us-east-1.
actionExecutionInput_region :: Lens.Lens' ActionExecutionInput (Prelude.Maybe Prelude.Text)
actionExecutionInput_region = Lens.lens (\ActionExecutionInput' {region} -> region) (\s@ActionExecutionInput' {} a -> s {region = a} :: ActionExecutionInput)

-- | Details of input artifacts of the action that correspond to the action
-- execution.
actionExecutionInput_inputArtifacts :: Lens.Lens' ActionExecutionInput (Prelude.Maybe [ArtifactDetail])
actionExecutionInput_inputArtifacts = Lens.lens (\ActionExecutionInput' {inputArtifacts} -> inputArtifacts) (\s@ActionExecutionInput' {} a -> s {inputArtifacts = a} :: ActionExecutionInput) Prelude.. Lens.mapping Lens.coerced

-- | The variable namespace associated with the action. All variables
-- produced as output by this action fall under this namespace.
actionExecutionInput_namespace :: Lens.Lens' ActionExecutionInput (Prelude.Maybe Prelude.Text)
actionExecutionInput_namespace = Lens.lens (\ActionExecutionInput' {namespace} -> namespace) (\s@ActionExecutionInput' {} a -> s {namespace = a} :: ActionExecutionInput)

instance Core.FromJSON ActionExecutionInput where
  parseJSON =
    Core.withObject
      "ActionExecutionInput"
      ( \x ->
          ActionExecutionInput'
            Prelude.<$> ( x Core..:? "resolvedConfiguration"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "roleArn")
            Prelude.<*> (x Core..:? "actionTypeId")
            Prelude.<*> (x Core..:? "configuration" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "region")
            Prelude.<*> (x Core..:? "inputArtifacts" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "namespace")
      )

instance Prelude.Hashable ActionExecutionInput where
  hashWithSalt _salt ActionExecutionInput' {..} =
    _salt `Prelude.hashWithSalt` resolvedConfiguration
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` actionTypeId
      `Prelude.hashWithSalt` configuration
      `Prelude.hashWithSalt` region
      `Prelude.hashWithSalt` inputArtifacts
      `Prelude.hashWithSalt` namespace

instance Prelude.NFData ActionExecutionInput where
  rnf ActionExecutionInput' {..} =
    Prelude.rnf resolvedConfiguration
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf actionTypeId
      `Prelude.seq` Prelude.rnf configuration
      `Prelude.seq` Prelude.rnf region
      `Prelude.seq` Prelude.rnf inputArtifacts
      `Prelude.seq` Prelude.rnf namespace

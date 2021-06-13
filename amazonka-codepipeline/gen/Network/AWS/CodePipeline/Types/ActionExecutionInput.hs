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
-- Module      : Network.AWS.CodePipeline.Types.ActionExecutionInput
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.ActionExecutionInput where

import Network.AWS.CodePipeline.Types.ActionTypeId
import Network.AWS.CodePipeline.Types.ArtifactDetail
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Input information used for an action execution.
--
-- /See:/ 'newActionExecutionInput' smart constructor.
data ActionExecutionInput = ActionExecutionInput'
  { -- | The ARN of the IAM service role that performs the declared action. This
    -- is assumed through the roleArn for the pipeline.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | Configuration data for an action execution.
    configuration :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Configuration data for an action execution with all variable references
    -- replaced with their real values for the execution.
    resolvedConfiguration :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The variable namespace associated with the action. All variables
    -- produced as output by this action fall under this namespace.
    namespace :: Prelude.Maybe Prelude.Text,
    actionTypeId :: Prelude.Maybe ActionTypeId,
    -- | Details of input artifacts of the action that correspond to the action
    -- execution.
    inputArtifacts :: Prelude.Maybe [ArtifactDetail],
    -- | The AWS Region for the action, such as us-east-1.
    region :: Prelude.Maybe Prelude.Text
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
-- 'roleArn', 'actionExecutionInput_roleArn' - The ARN of the IAM service role that performs the declared action. This
-- is assumed through the roleArn for the pipeline.
--
-- 'configuration', 'actionExecutionInput_configuration' - Configuration data for an action execution.
--
-- 'resolvedConfiguration', 'actionExecutionInput_resolvedConfiguration' - Configuration data for an action execution with all variable references
-- replaced with their real values for the execution.
--
-- 'namespace', 'actionExecutionInput_namespace' - The variable namespace associated with the action. All variables
-- produced as output by this action fall under this namespace.
--
-- 'actionTypeId', 'actionExecutionInput_actionTypeId' - Undocumented member.
--
-- 'inputArtifacts', 'actionExecutionInput_inputArtifacts' - Details of input artifacts of the action that correspond to the action
-- execution.
--
-- 'region', 'actionExecutionInput_region' - The AWS Region for the action, such as us-east-1.
newActionExecutionInput ::
  ActionExecutionInput
newActionExecutionInput =
  ActionExecutionInput'
    { roleArn = Prelude.Nothing,
      configuration = Prelude.Nothing,
      resolvedConfiguration = Prelude.Nothing,
      namespace = Prelude.Nothing,
      actionTypeId = Prelude.Nothing,
      inputArtifacts = Prelude.Nothing,
      region = Prelude.Nothing
    }

-- | The ARN of the IAM service role that performs the declared action. This
-- is assumed through the roleArn for the pipeline.
actionExecutionInput_roleArn :: Lens.Lens' ActionExecutionInput (Prelude.Maybe Prelude.Text)
actionExecutionInput_roleArn = Lens.lens (\ActionExecutionInput' {roleArn} -> roleArn) (\s@ActionExecutionInput' {} a -> s {roleArn = a} :: ActionExecutionInput)

-- | Configuration data for an action execution.
actionExecutionInput_configuration :: Lens.Lens' ActionExecutionInput (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
actionExecutionInput_configuration = Lens.lens (\ActionExecutionInput' {configuration} -> configuration) (\s@ActionExecutionInput' {} a -> s {configuration = a} :: ActionExecutionInput) Prelude.. Lens.mapping Lens._Coerce

-- | Configuration data for an action execution with all variable references
-- replaced with their real values for the execution.
actionExecutionInput_resolvedConfiguration :: Lens.Lens' ActionExecutionInput (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
actionExecutionInput_resolvedConfiguration = Lens.lens (\ActionExecutionInput' {resolvedConfiguration} -> resolvedConfiguration) (\s@ActionExecutionInput' {} a -> s {resolvedConfiguration = a} :: ActionExecutionInput) Prelude.. Lens.mapping Lens._Coerce

-- | The variable namespace associated with the action. All variables
-- produced as output by this action fall under this namespace.
actionExecutionInput_namespace :: Lens.Lens' ActionExecutionInput (Prelude.Maybe Prelude.Text)
actionExecutionInput_namespace = Lens.lens (\ActionExecutionInput' {namespace} -> namespace) (\s@ActionExecutionInput' {} a -> s {namespace = a} :: ActionExecutionInput)

-- | Undocumented member.
actionExecutionInput_actionTypeId :: Lens.Lens' ActionExecutionInput (Prelude.Maybe ActionTypeId)
actionExecutionInput_actionTypeId = Lens.lens (\ActionExecutionInput' {actionTypeId} -> actionTypeId) (\s@ActionExecutionInput' {} a -> s {actionTypeId = a} :: ActionExecutionInput)

-- | Details of input artifacts of the action that correspond to the action
-- execution.
actionExecutionInput_inputArtifacts :: Lens.Lens' ActionExecutionInput (Prelude.Maybe [ArtifactDetail])
actionExecutionInput_inputArtifacts = Lens.lens (\ActionExecutionInput' {inputArtifacts} -> inputArtifacts) (\s@ActionExecutionInput' {} a -> s {inputArtifacts = a} :: ActionExecutionInput) Prelude.. Lens.mapping Lens._Coerce

-- | The AWS Region for the action, such as us-east-1.
actionExecutionInput_region :: Lens.Lens' ActionExecutionInput (Prelude.Maybe Prelude.Text)
actionExecutionInput_region = Lens.lens (\ActionExecutionInput' {region} -> region) (\s@ActionExecutionInput' {} a -> s {region = a} :: ActionExecutionInput)

instance Core.FromJSON ActionExecutionInput where
  parseJSON =
    Core.withObject
      "ActionExecutionInput"
      ( \x ->
          ActionExecutionInput'
            Prelude.<$> (x Core..:? "roleArn")
            Prelude.<*> (x Core..:? "configuration" Core..!= Prelude.mempty)
            Prelude.<*> ( x Core..:? "resolvedConfiguration"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "namespace")
            Prelude.<*> (x Core..:? "actionTypeId")
            Prelude.<*> (x Core..:? "inputArtifacts" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "region")
      )

instance Prelude.Hashable ActionExecutionInput

instance Prelude.NFData ActionExecutionInput

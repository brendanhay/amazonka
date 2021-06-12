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

-- | Input information used for an action execution.
--
-- /See:/ 'newActionExecutionInput' smart constructor.
data ActionExecutionInput = ActionExecutionInput'
  { -- | The ARN of the IAM service role that performs the declared action. This
    -- is assumed through the roleArn for the pipeline.
    roleArn :: Core.Maybe Core.Text,
    -- | Configuration data for an action execution.
    configuration :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | Configuration data for an action execution with all variable references
    -- replaced with their real values for the execution.
    resolvedConfiguration :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The variable namespace associated with the action. All variables
    -- produced as output by this action fall under this namespace.
    namespace :: Core.Maybe Core.Text,
    actionTypeId :: Core.Maybe ActionTypeId,
    -- | Details of input artifacts of the action that correspond to the action
    -- execution.
    inputArtifacts :: Core.Maybe [ArtifactDetail],
    -- | The AWS Region for the action, such as us-east-1.
    region :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { roleArn = Core.Nothing,
      configuration = Core.Nothing,
      resolvedConfiguration = Core.Nothing,
      namespace = Core.Nothing,
      actionTypeId = Core.Nothing,
      inputArtifacts = Core.Nothing,
      region = Core.Nothing
    }

-- | The ARN of the IAM service role that performs the declared action. This
-- is assumed through the roleArn for the pipeline.
actionExecutionInput_roleArn :: Lens.Lens' ActionExecutionInput (Core.Maybe Core.Text)
actionExecutionInput_roleArn = Lens.lens (\ActionExecutionInput' {roleArn} -> roleArn) (\s@ActionExecutionInput' {} a -> s {roleArn = a} :: ActionExecutionInput)

-- | Configuration data for an action execution.
actionExecutionInput_configuration :: Lens.Lens' ActionExecutionInput (Core.Maybe (Core.HashMap Core.Text Core.Text))
actionExecutionInput_configuration = Lens.lens (\ActionExecutionInput' {configuration} -> configuration) (\s@ActionExecutionInput' {} a -> s {configuration = a} :: ActionExecutionInput) Core.. Lens.mapping Lens._Coerce

-- | Configuration data for an action execution with all variable references
-- replaced with their real values for the execution.
actionExecutionInput_resolvedConfiguration :: Lens.Lens' ActionExecutionInput (Core.Maybe (Core.HashMap Core.Text Core.Text))
actionExecutionInput_resolvedConfiguration = Lens.lens (\ActionExecutionInput' {resolvedConfiguration} -> resolvedConfiguration) (\s@ActionExecutionInput' {} a -> s {resolvedConfiguration = a} :: ActionExecutionInput) Core.. Lens.mapping Lens._Coerce

-- | The variable namespace associated with the action. All variables
-- produced as output by this action fall under this namespace.
actionExecutionInput_namespace :: Lens.Lens' ActionExecutionInput (Core.Maybe Core.Text)
actionExecutionInput_namespace = Lens.lens (\ActionExecutionInput' {namespace} -> namespace) (\s@ActionExecutionInput' {} a -> s {namespace = a} :: ActionExecutionInput)

-- | Undocumented member.
actionExecutionInput_actionTypeId :: Lens.Lens' ActionExecutionInput (Core.Maybe ActionTypeId)
actionExecutionInput_actionTypeId = Lens.lens (\ActionExecutionInput' {actionTypeId} -> actionTypeId) (\s@ActionExecutionInput' {} a -> s {actionTypeId = a} :: ActionExecutionInput)

-- | Details of input artifacts of the action that correspond to the action
-- execution.
actionExecutionInput_inputArtifacts :: Lens.Lens' ActionExecutionInput (Core.Maybe [ArtifactDetail])
actionExecutionInput_inputArtifacts = Lens.lens (\ActionExecutionInput' {inputArtifacts} -> inputArtifacts) (\s@ActionExecutionInput' {} a -> s {inputArtifacts = a} :: ActionExecutionInput) Core.. Lens.mapping Lens._Coerce

-- | The AWS Region for the action, such as us-east-1.
actionExecutionInput_region :: Lens.Lens' ActionExecutionInput (Core.Maybe Core.Text)
actionExecutionInput_region = Lens.lens (\ActionExecutionInput' {region} -> region) (\s@ActionExecutionInput' {} a -> s {region = a} :: ActionExecutionInput)

instance Core.FromJSON ActionExecutionInput where
  parseJSON =
    Core.withObject
      "ActionExecutionInput"
      ( \x ->
          ActionExecutionInput'
            Core.<$> (x Core..:? "roleArn")
            Core.<*> (x Core..:? "configuration" Core..!= Core.mempty)
            Core.<*> ( x Core..:? "resolvedConfiguration"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "namespace")
            Core.<*> (x Core..:? "actionTypeId")
            Core.<*> (x Core..:? "inputArtifacts" Core..!= Core.mempty)
            Core.<*> (x Core..:? "region")
      )

instance Core.Hashable ActionExecutionInput

instance Core.NFData ActionExecutionInput

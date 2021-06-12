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
-- Module      : Network.AWS.CodePipeline.Types.ActionTypeDeclaration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.ActionTypeDeclaration where

import Network.AWS.CodePipeline.Types.ActionTypeArtifactDetails
import Network.AWS.CodePipeline.Types.ActionTypeExecutor
import Network.AWS.CodePipeline.Types.ActionTypeIdentifier
import Network.AWS.CodePipeline.Types.ActionTypePermissions
import Network.AWS.CodePipeline.Types.ActionTypeProperty
import Network.AWS.CodePipeline.Types.ActionTypeUrls
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The parameters for the action type definition that are provided when the
-- action type is created or updated.
--
-- /See:/ 'newActionTypeDeclaration' smart constructor.
data ActionTypeDeclaration = ActionTypeDeclaration'
  { -- | Details identifying the accounts with permissions to use the action
    -- type.
    permissions :: Core.Maybe ActionTypePermissions,
    -- | The links associated with the action type to be updated.
    urls :: Core.Maybe ActionTypeUrls,
    -- | The properties of the action type to be updated.
    properties :: Core.Maybe [ActionTypeProperty],
    -- | The description for the action type to be updated.
    description :: Core.Maybe Core.Text,
    -- | Information about the executor for an action type that was created with
    -- any supported integration model.
    executor :: ActionTypeExecutor,
    -- | The action category, owner, provider, and version of the action type to
    -- be updated.
    id :: ActionTypeIdentifier,
    -- | Details for the artifacts, such as application files, to be worked on by
    -- the action. For example, the minimum and maximum number of input
    -- artifacts allowed.
    inputArtifactDetails :: ActionTypeArtifactDetails,
    -- | Details for the output artifacts, such as a built application, that are
    -- the result of the action. For example, the minimum and maximum number of
    -- output artifacts allowed.
    outputArtifactDetails :: ActionTypeArtifactDetails
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ActionTypeDeclaration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'permissions', 'actionTypeDeclaration_permissions' - Details identifying the accounts with permissions to use the action
-- type.
--
-- 'urls', 'actionTypeDeclaration_urls' - The links associated with the action type to be updated.
--
-- 'properties', 'actionTypeDeclaration_properties' - The properties of the action type to be updated.
--
-- 'description', 'actionTypeDeclaration_description' - The description for the action type to be updated.
--
-- 'executor', 'actionTypeDeclaration_executor' - Information about the executor for an action type that was created with
-- any supported integration model.
--
-- 'id', 'actionTypeDeclaration_id' - The action category, owner, provider, and version of the action type to
-- be updated.
--
-- 'inputArtifactDetails', 'actionTypeDeclaration_inputArtifactDetails' - Details for the artifacts, such as application files, to be worked on by
-- the action. For example, the minimum and maximum number of input
-- artifacts allowed.
--
-- 'outputArtifactDetails', 'actionTypeDeclaration_outputArtifactDetails' - Details for the output artifacts, such as a built application, that are
-- the result of the action. For example, the minimum and maximum number of
-- output artifacts allowed.
newActionTypeDeclaration ::
  -- | 'executor'
  ActionTypeExecutor ->
  -- | 'id'
  ActionTypeIdentifier ->
  -- | 'inputArtifactDetails'
  ActionTypeArtifactDetails ->
  -- | 'outputArtifactDetails'
  ActionTypeArtifactDetails ->
  ActionTypeDeclaration
newActionTypeDeclaration
  pExecutor_
  pId_
  pInputArtifactDetails_
  pOutputArtifactDetails_ =
    ActionTypeDeclaration'
      { permissions = Core.Nothing,
        urls = Core.Nothing,
        properties = Core.Nothing,
        description = Core.Nothing,
        executor = pExecutor_,
        id = pId_,
        inputArtifactDetails = pInputArtifactDetails_,
        outputArtifactDetails = pOutputArtifactDetails_
      }

-- | Details identifying the accounts with permissions to use the action
-- type.
actionTypeDeclaration_permissions :: Lens.Lens' ActionTypeDeclaration (Core.Maybe ActionTypePermissions)
actionTypeDeclaration_permissions = Lens.lens (\ActionTypeDeclaration' {permissions} -> permissions) (\s@ActionTypeDeclaration' {} a -> s {permissions = a} :: ActionTypeDeclaration)

-- | The links associated with the action type to be updated.
actionTypeDeclaration_urls :: Lens.Lens' ActionTypeDeclaration (Core.Maybe ActionTypeUrls)
actionTypeDeclaration_urls = Lens.lens (\ActionTypeDeclaration' {urls} -> urls) (\s@ActionTypeDeclaration' {} a -> s {urls = a} :: ActionTypeDeclaration)

-- | The properties of the action type to be updated.
actionTypeDeclaration_properties :: Lens.Lens' ActionTypeDeclaration (Core.Maybe [ActionTypeProperty])
actionTypeDeclaration_properties = Lens.lens (\ActionTypeDeclaration' {properties} -> properties) (\s@ActionTypeDeclaration' {} a -> s {properties = a} :: ActionTypeDeclaration) Core.. Lens.mapping Lens._Coerce

-- | The description for the action type to be updated.
actionTypeDeclaration_description :: Lens.Lens' ActionTypeDeclaration (Core.Maybe Core.Text)
actionTypeDeclaration_description = Lens.lens (\ActionTypeDeclaration' {description} -> description) (\s@ActionTypeDeclaration' {} a -> s {description = a} :: ActionTypeDeclaration)

-- | Information about the executor for an action type that was created with
-- any supported integration model.
actionTypeDeclaration_executor :: Lens.Lens' ActionTypeDeclaration ActionTypeExecutor
actionTypeDeclaration_executor = Lens.lens (\ActionTypeDeclaration' {executor} -> executor) (\s@ActionTypeDeclaration' {} a -> s {executor = a} :: ActionTypeDeclaration)

-- | The action category, owner, provider, and version of the action type to
-- be updated.
actionTypeDeclaration_id :: Lens.Lens' ActionTypeDeclaration ActionTypeIdentifier
actionTypeDeclaration_id = Lens.lens (\ActionTypeDeclaration' {id} -> id) (\s@ActionTypeDeclaration' {} a -> s {id = a} :: ActionTypeDeclaration)

-- | Details for the artifacts, such as application files, to be worked on by
-- the action. For example, the minimum and maximum number of input
-- artifacts allowed.
actionTypeDeclaration_inputArtifactDetails :: Lens.Lens' ActionTypeDeclaration ActionTypeArtifactDetails
actionTypeDeclaration_inputArtifactDetails = Lens.lens (\ActionTypeDeclaration' {inputArtifactDetails} -> inputArtifactDetails) (\s@ActionTypeDeclaration' {} a -> s {inputArtifactDetails = a} :: ActionTypeDeclaration)

-- | Details for the output artifacts, such as a built application, that are
-- the result of the action. For example, the minimum and maximum number of
-- output artifacts allowed.
actionTypeDeclaration_outputArtifactDetails :: Lens.Lens' ActionTypeDeclaration ActionTypeArtifactDetails
actionTypeDeclaration_outputArtifactDetails = Lens.lens (\ActionTypeDeclaration' {outputArtifactDetails} -> outputArtifactDetails) (\s@ActionTypeDeclaration' {} a -> s {outputArtifactDetails = a} :: ActionTypeDeclaration)

instance Core.FromJSON ActionTypeDeclaration where
  parseJSON =
    Core.withObject
      "ActionTypeDeclaration"
      ( \x ->
          ActionTypeDeclaration'
            Core.<$> (x Core..:? "permissions")
            Core.<*> (x Core..:? "urls")
            Core.<*> (x Core..:? "properties" Core..!= Core.mempty)
            Core.<*> (x Core..:? "description")
            Core.<*> (x Core..: "executor")
            Core.<*> (x Core..: "id")
            Core.<*> (x Core..: "inputArtifactDetails")
            Core.<*> (x Core..: "outputArtifactDetails")
      )

instance Core.Hashable ActionTypeDeclaration

instance Core.NFData ActionTypeDeclaration

instance Core.ToJSON ActionTypeDeclaration where
  toJSON ActionTypeDeclaration' {..} =
    Core.object
      ( Core.catMaybes
          [ ("permissions" Core..=) Core.<$> permissions,
            ("urls" Core..=) Core.<$> urls,
            ("properties" Core..=) Core.<$> properties,
            ("description" Core..=) Core.<$> description,
            Core.Just ("executor" Core..= executor),
            Core.Just ("id" Core..= id),
            Core.Just
              ( "inputArtifactDetails"
                  Core..= inputArtifactDetails
              ),
            Core.Just
              ( "outputArtifactDetails"
                  Core..= outputArtifactDetails
              )
          ]
      )

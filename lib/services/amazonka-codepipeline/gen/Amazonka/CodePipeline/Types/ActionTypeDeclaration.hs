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
-- Module      : Amazonka.CodePipeline.Types.ActionTypeDeclaration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodePipeline.Types.ActionTypeDeclaration where

import Amazonka.CodePipeline.Types.ActionTypeArtifactDetails
import Amazonka.CodePipeline.Types.ActionTypeExecutor
import Amazonka.CodePipeline.Types.ActionTypeIdentifier
import Amazonka.CodePipeline.Types.ActionTypePermissions
import Amazonka.CodePipeline.Types.ActionTypeProperty
import Amazonka.CodePipeline.Types.ActionTypeUrls
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The parameters for the action type definition that are provided when the
-- action type is created or updated.
--
-- /See:/ 'newActionTypeDeclaration' smart constructor.
data ActionTypeDeclaration = ActionTypeDeclaration'
  { -- | The description for the action type to be updated.
    description :: Prelude.Maybe Prelude.Text,
    -- | Details identifying the accounts with permissions to use the action
    -- type.
    permissions :: Prelude.Maybe ActionTypePermissions,
    -- | The properties of the action type to be updated.
    properties :: Prelude.Maybe [ActionTypeProperty],
    -- | The links associated with the action type to be updated.
    urls :: Prelude.Maybe ActionTypeUrls,
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ActionTypeDeclaration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'actionTypeDeclaration_description' - The description for the action type to be updated.
--
-- 'permissions', 'actionTypeDeclaration_permissions' - Details identifying the accounts with permissions to use the action
-- type.
--
-- 'properties', 'actionTypeDeclaration_properties' - The properties of the action type to be updated.
--
-- 'urls', 'actionTypeDeclaration_urls' - The links associated with the action type to be updated.
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
      { description =
          Prelude.Nothing,
        permissions = Prelude.Nothing,
        properties = Prelude.Nothing,
        urls = Prelude.Nothing,
        executor = pExecutor_,
        id = pId_,
        inputArtifactDetails = pInputArtifactDetails_,
        outputArtifactDetails = pOutputArtifactDetails_
      }

-- | The description for the action type to be updated.
actionTypeDeclaration_description :: Lens.Lens' ActionTypeDeclaration (Prelude.Maybe Prelude.Text)
actionTypeDeclaration_description = Lens.lens (\ActionTypeDeclaration' {description} -> description) (\s@ActionTypeDeclaration' {} a -> s {description = a} :: ActionTypeDeclaration)

-- | Details identifying the accounts with permissions to use the action
-- type.
actionTypeDeclaration_permissions :: Lens.Lens' ActionTypeDeclaration (Prelude.Maybe ActionTypePermissions)
actionTypeDeclaration_permissions = Lens.lens (\ActionTypeDeclaration' {permissions} -> permissions) (\s@ActionTypeDeclaration' {} a -> s {permissions = a} :: ActionTypeDeclaration)

-- | The properties of the action type to be updated.
actionTypeDeclaration_properties :: Lens.Lens' ActionTypeDeclaration (Prelude.Maybe [ActionTypeProperty])
actionTypeDeclaration_properties = Lens.lens (\ActionTypeDeclaration' {properties} -> properties) (\s@ActionTypeDeclaration' {} a -> s {properties = a} :: ActionTypeDeclaration) Prelude.. Lens.mapping Lens.coerced

-- | The links associated with the action type to be updated.
actionTypeDeclaration_urls :: Lens.Lens' ActionTypeDeclaration (Prelude.Maybe ActionTypeUrls)
actionTypeDeclaration_urls = Lens.lens (\ActionTypeDeclaration' {urls} -> urls) (\s@ActionTypeDeclaration' {} a -> s {urls = a} :: ActionTypeDeclaration)

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

instance Data.FromJSON ActionTypeDeclaration where
  parseJSON =
    Data.withObject
      "ActionTypeDeclaration"
      ( \x ->
          ActionTypeDeclaration'
            Prelude.<$> (x Data..:? "description")
            Prelude.<*> (x Data..:? "permissions")
            Prelude.<*> (x Data..:? "properties" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "urls")
            Prelude.<*> (x Data..: "executor")
            Prelude.<*> (x Data..: "id")
            Prelude.<*> (x Data..: "inputArtifactDetails")
            Prelude.<*> (x Data..: "outputArtifactDetails")
      )

instance Prelude.Hashable ActionTypeDeclaration where
  hashWithSalt _salt ActionTypeDeclaration' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` permissions
      `Prelude.hashWithSalt` properties
      `Prelude.hashWithSalt` urls
      `Prelude.hashWithSalt` executor
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` inputArtifactDetails
      `Prelude.hashWithSalt` outputArtifactDetails

instance Prelude.NFData ActionTypeDeclaration where
  rnf ActionTypeDeclaration' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf permissions
      `Prelude.seq` Prelude.rnf properties
      `Prelude.seq` Prelude.rnf urls
      `Prelude.seq` Prelude.rnf executor
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf inputArtifactDetails
      `Prelude.seq` Prelude.rnf outputArtifactDetails

instance Data.ToJSON ActionTypeDeclaration where
  toJSON ActionTypeDeclaration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("description" Data..=) Prelude.<$> description,
            ("permissions" Data..=) Prelude.<$> permissions,
            ("properties" Data..=) Prelude.<$> properties,
            ("urls" Data..=) Prelude.<$> urls,
            Prelude.Just ("executor" Data..= executor),
            Prelude.Just ("id" Data..= id),
            Prelude.Just
              ( "inputArtifactDetails"
                  Data..= inputArtifactDetails
              ),
            Prelude.Just
              ( "outputArtifactDetails"
                  Data..= outputArtifactDetails
              )
          ]
      )

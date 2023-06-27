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
-- Module      : Amazonka.CodePipeline.Types.ActionTypeId
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodePipeline.Types.ActionTypeId where

import Amazonka.CodePipeline.Types.ActionCategory
import Amazonka.CodePipeline.Types.ActionOwner
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents information about an action type.
--
-- /See:/ 'newActionTypeId' smart constructor.
data ActionTypeId = ActionTypeId'
  { -- | A category defines what kind of action can be taken in the stage, and
    -- constrains the provider type for the action. Valid categories are
    -- limited to one of the following values.
    --
    -- -   Source
    --
    -- -   Build
    --
    -- -   Test
    --
    -- -   Deploy
    --
    -- -   Invoke
    --
    -- -   Approval
    category :: ActionCategory,
    -- | The creator of the action being called. There are three valid values for
    -- the @Owner@ field in the action category section within your pipeline
    -- structure: @AWS@, @ThirdParty@, and @Custom@. For more information, see
    -- <https://docs.aws.amazon.com/codepipeline/latest/userguide/reference-pipeline-structure.html#actions-valid-providers Valid Action Types and Providers in CodePipeline>.
    owner :: ActionOwner,
    -- | The provider of the service being called by the action. Valid providers
    -- are determined by the action category. For example, an action in the
    -- Deploy category type might have a provider of CodeDeploy, which would be
    -- specified as @CodeDeploy@. For more information, see
    -- <https://docs.aws.amazon.com/codepipeline/latest/userguide/reference-pipeline-structure.html#actions-valid-providers Valid Action Types and Providers in CodePipeline>.
    provider :: Prelude.Text,
    -- | A string that describes the action version.
    version :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ActionTypeId' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'category', 'actionTypeId_category' - A category defines what kind of action can be taken in the stage, and
-- constrains the provider type for the action. Valid categories are
-- limited to one of the following values.
--
-- -   Source
--
-- -   Build
--
-- -   Test
--
-- -   Deploy
--
-- -   Invoke
--
-- -   Approval
--
-- 'owner', 'actionTypeId_owner' - The creator of the action being called. There are three valid values for
-- the @Owner@ field in the action category section within your pipeline
-- structure: @AWS@, @ThirdParty@, and @Custom@. For more information, see
-- <https://docs.aws.amazon.com/codepipeline/latest/userguide/reference-pipeline-structure.html#actions-valid-providers Valid Action Types and Providers in CodePipeline>.
--
-- 'provider', 'actionTypeId_provider' - The provider of the service being called by the action. Valid providers
-- are determined by the action category. For example, an action in the
-- Deploy category type might have a provider of CodeDeploy, which would be
-- specified as @CodeDeploy@. For more information, see
-- <https://docs.aws.amazon.com/codepipeline/latest/userguide/reference-pipeline-structure.html#actions-valid-providers Valid Action Types and Providers in CodePipeline>.
--
-- 'version', 'actionTypeId_version' - A string that describes the action version.
newActionTypeId ::
  -- | 'category'
  ActionCategory ->
  -- | 'owner'
  ActionOwner ->
  -- | 'provider'
  Prelude.Text ->
  -- | 'version'
  Prelude.Text ->
  ActionTypeId
newActionTypeId
  pCategory_
  pOwner_
  pProvider_
  pVersion_ =
    ActionTypeId'
      { category = pCategory_,
        owner = pOwner_,
        provider = pProvider_,
        version = pVersion_
      }

-- | A category defines what kind of action can be taken in the stage, and
-- constrains the provider type for the action. Valid categories are
-- limited to one of the following values.
--
-- -   Source
--
-- -   Build
--
-- -   Test
--
-- -   Deploy
--
-- -   Invoke
--
-- -   Approval
actionTypeId_category :: Lens.Lens' ActionTypeId ActionCategory
actionTypeId_category = Lens.lens (\ActionTypeId' {category} -> category) (\s@ActionTypeId' {} a -> s {category = a} :: ActionTypeId)

-- | The creator of the action being called. There are three valid values for
-- the @Owner@ field in the action category section within your pipeline
-- structure: @AWS@, @ThirdParty@, and @Custom@. For more information, see
-- <https://docs.aws.amazon.com/codepipeline/latest/userguide/reference-pipeline-structure.html#actions-valid-providers Valid Action Types and Providers in CodePipeline>.
actionTypeId_owner :: Lens.Lens' ActionTypeId ActionOwner
actionTypeId_owner = Lens.lens (\ActionTypeId' {owner} -> owner) (\s@ActionTypeId' {} a -> s {owner = a} :: ActionTypeId)

-- | The provider of the service being called by the action. Valid providers
-- are determined by the action category. For example, an action in the
-- Deploy category type might have a provider of CodeDeploy, which would be
-- specified as @CodeDeploy@. For more information, see
-- <https://docs.aws.amazon.com/codepipeline/latest/userguide/reference-pipeline-structure.html#actions-valid-providers Valid Action Types and Providers in CodePipeline>.
actionTypeId_provider :: Lens.Lens' ActionTypeId Prelude.Text
actionTypeId_provider = Lens.lens (\ActionTypeId' {provider} -> provider) (\s@ActionTypeId' {} a -> s {provider = a} :: ActionTypeId)

-- | A string that describes the action version.
actionTypeId_version :: Lens.Lens' ActionTypeId Prelude.Text
actionTypeId_version = Lens.lens (\ActionTypeId' {version} -> version) (\s@ActionTypeId' {} a -> s {version = a} :: ActionTypeId)

instance Data.FromJSON ActionTypeId where
  parseJSON =
    Data.withObject
      "ActionTypeId"
      ( \x ->
          ActionTypeId'
            Prelude.<$> (x Data..: "category")
            Prelude.<*> (x Data..: "owner")
            Prelude.<*> (x Data..: "provider")
            Prelude.<*> (x Data..: "version")
      )

instance Prelude.Hashable ActionTypeId where
  hashWithSalt _salt ActionTypeId' {..} =
    _salt
      `Prelude.hashWithSalt` category
      `Prelude.hashWithSalt` owner
      `Prelude.hashWithSalt` provider
      `Prelude.hashWithSalt` version

instance Prelude.NFData ActionTypeId where
  rnf ActionTypeId' {..} =
    Prelude.rnf category
      `Prelude.seq` Prelude.rnf owner
      `Prelude.seq` Prelude.rnf provider
      `Prelude.seq` Prelude.rnf version

instance Data.ToJSON ActionTypeId where
  toJSON ActionTypeId' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("category" Data..= category),
            Prelude.Just ("owner" Data..= owner),
            Prelude.Just ("provider" Data..= provider),
            Prelude.Just ("version" Data..= version)
          ]
      )

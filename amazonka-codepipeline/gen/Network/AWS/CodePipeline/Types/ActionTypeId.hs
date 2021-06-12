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
-- Module      : Network.AWS.CodePipeline.Types.ActionTypeId
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.ActionTypeId where

import Network.AWS.CodePipeline.Types.ActionCategory
import Network.AWS.CodePipeline.Types.ActionOwner
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

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
    -- Deploy category type might have a provider of AWS CodeDeploy, which
    -- would be specified as CodeDeploy. For more information, see
    -- <https://docs.aws.amazon.com/codepipeline/latest/userguide/reference-pipeline-structure.html#actions-valid-providers Valid Action Types and Providers in CodePipeline>.
    provider :: Core.Text,
    -- | A string that describes the action version.
    version :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- Deploy category type might have a provider of AWS CodeDeploy, which
-- would be specified as CodeDeploy. For more information, see
-- <https://docs.aws.amazon.com/codepipeline/latest/userguide/reference-pipeline-structure.html#actions-valid-providers Valid Action Types and Providers in CodePipeline>.
--
-- 'version', 'actionTypeId_version' - A string that describes the action version.
newActionTypeId ::
  -- | 'category'
  ActionCategory ->
  -- | 'owner'
  ActionOwner ->
  -- | 'provider'
  Core.Text ->
  -- | 'version'
  Core.Text ->
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
-- Deploy category type might have a provider of AWS CodeDeploy, which
-- would be specified as CodeDeploy. For more information, see
-- <https://docs.aws.amazon.com/codepipeline/latest/userguide/reference-pipeline-structure.html#actions-valid-providers Valid Action Types and Providers in CodePipeline>.
actionTypeId_provider :: Lens.Lens' ActionTypeId Core.Text
actionTypeId_provider = Lens.lens (\ActionTypeId' {provider} -> provider) (\s@ActionTypeId' {} a -> s {provider = a} :: ActionTypeId)

-- | A string that describes the action version.
actionTypeId_version :: Lens.Lens' ActionTypeId Core.Text
actionTypeId_version = Lens.lens (\ActionTypeId' {version} -> version) (\s@ActionTypeId' {} a -> s {version = a} :: ActionTypeId)

instance Core.FromJSON ActionTypeId where
  parseJSON =
    Core.withObject
      "ActionTypeId"
      ( \x ->
          ActionTypeId'
            Core.<$> (x Core..: "category")
            Core.<*> (x Core..: "owner")
            Core.<*> (x Core..: "provider")
            Core.<*> (x Core..: "version")
      )

instance Core.Hashable ActionTypeId

instance Core.NFData ActionTypeId

instance Core.ToJSON ActionTypeId where
  toJSON ActionTypeId' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("category" Core..= category),
            Core.Just ("owner" Core..= owner),
            Core.Just ("provider" Core..= provider),
            Core.Just ("version" Core..= version)
          ]
      )

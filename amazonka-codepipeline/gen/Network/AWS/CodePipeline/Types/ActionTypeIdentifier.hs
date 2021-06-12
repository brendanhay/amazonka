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
-- Module      : Network.AWS.CodePipeline.Types.ActionTypeIdentifier
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.ActionTypeIdentifier where

import Network.AWS.CodePipeline.Types.ActionCategory
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Specifies the category, owner, provider, and version of the action type.
--
-- /See:/ 'newActionTypeIdentifier' smart constructor.
data ActionTypeIdentifier = ActionTypeIdentifier'
  { -- | Defines what kind of action can be taken in the stage, one of the
    -- following:
    --
    -- -   @Source@
    --
    -- -   @Build@
    --
    -- -   @Test@
    --
    -- -   @Deploy@
    --
    -- -   @Approval@
    --
    -- -   @Invoke@
    category :: ActionCategory,
    -- | The creator of the action type being called: @AWS@ or @ThirdParty@.
    owner :: Core.Text,
    -- | The provider of the action type being called. The provider name is
    -- supplied when the action type is created.
    provider :: Core.Text,
    -- | A string that describes the action type version.
    version :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ActionTypeIdentifier' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'category', 'actionTypeIdentifier_category' - Defines what kind of action can be taken in the stage, one of the
-- following:
--
-- -   @Source@
--
-- -   @Build@
--
-- -   @Test@
--
-- -   @Deploy@
--
-- -   @Approval@
--
-- -   @Invoke@
--
-- 'owner', 'actionTypeIdentifier_owner' - The creator of the action type being called: @AWS@ or @ThirdParty@.
--
-- 'provider', 'actionTypeIdentifier_provider' - The provider of the action type being called. The provider name is
-- supplied when the action type is created.
--
-- 'version', 'actionTypeIdentifier_version' - A string that describes the action type version.
newActionTypeIdentifier ::
  -- | 'category'
  ActionCategory ->
  -- | 'owner'
  Core.Text ->
  -- | 'provider'
  Core.Text ->
  -- | 'version'
  Core.Text ->
  ActionTypeIdentifier
newActionTypeIdentifier
  pCategory_
  pOwner_
  pProvider_
  pVersion_ =
    ActionTypeIdentifier'
      { category = pCategory_,
        owner = pOwner_,
        provider = pProvider_,
        version = pVersion_
      }

-- | Defines what kind of action can be taken in the stage, one of the
-- following:
--
-- -   @Source@
--
-- -   @Build@
--
-- -   @Test@
--
-- -   @Deploy@
--
-- -   @Approval@
--
-- -   @Invoke@
actionTypeIdentifier_category :: Lens.Lens' ActionTypeIdentifier ActionCategory
actionTypeIdentifier_category = Lens.lens (\ActionTypeIdentifier' {category} -> category) (\s@ActionTypeIdentifier' {} a -> s {category = a} :: ActionTypeIdentifier)

-- | The creator of the action type being called: @AWS@ or @ThirdParty@.
actionTypeIdentifier_owner :: Lens.Lens' ActionTypeIdentifier Core.Text
actionTypeIdentifier_owner = Lens.lens (\ActionTypeIdentifier' {owner} -> owner) (\s@ActionTypeIdentifier' {} a -> s {owner = a} :: ActionTypeIdentifier)

-- | The provider of the action type being called. The provider name is
-- supplied when the action type is created.
actionTypeIdentifier_provider :: Lens.Lens' ActionTypeIdentifier Core.Text
actionTypeIdentifier_provider = Lens.lens (\ActionTypeIdentifier' {provider} -> provider) (\s@ActionTypeIdentifier' {} a -> s {provider = a} :: ActionTypeIdentifier)

-- | A string that describes the action type version.
actionTypeIdentifier_version :: Lens.Lens' ActionTypeIdentifier Core.Text
actionTypeIdentifier_version = Lens.lens (\ActionTypeIdentifier' {version} -> version) (\s@ActionTypeIdentifier' {} a -> s {version = a} :: ActionTypeIdentifier)

instance Core.FromJSON ActionTypeIdentifier where
  parseJSON =
    Core.withObject
      "ActionTypeIdentifier"
      ( \x ->
          ActionTypeIdentifier'
            Core.<$> (x Core..: "category")
            Core.<*> (x Core..: "owner")
            Core.<*> (x Core..: "provider")
            Core.<*> (x Core..: "version")
      )

instance Core.Hashable ActionTypeIdentifier

instance Core.NFData ActionTypeIdentifier

instance Core.ToJSON ActionTypeIdentifier where
  toJSON ActionTypeIdentifier' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("category" Core..= category),
            Core.Just ("owner" Core..= owner),
            Core.Just ("provider" Core..= provider),
            Core.Just ("version" Core..= version)
          ]
      )

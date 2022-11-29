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
-- Module      : Amazonka.CodePipeline.Types.ActionTypeIdentifier
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodePipeline.Types.ActionTypeIdentifier where

import Amazonka.CodePipeline.Types.ActionCategory
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

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
    owner :: Prelude.Text,
    -- | The provider of the action type being called. The provider name is
    -- supplied when the action type is created.
    provider :: Prelude.Text,
    -- | A string that describes the action type version.
    version :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'provider'
  Prelude.Text ->
  -- | 'version'
  Prelude.Text ->
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
actionTypeIdentifier_owner :: Lens.Lens' ActionTypeIdentifier Prelude.Text
actionTypeIdentifier_owner = Lens.lens (\ActionTypeIdentifier' {owner} -> owner) (\s@ActionTypeIdentifier' {} a -> s {owner = a} :: ActionTypeIdentifier)

-- | The provider of the action type being called. The provider name is
-- supplied when the action type is created.
actionTypeIdentifier_provider :: Lens.Lens' ActionTypeIdentifier Prelude.Text
actionTypeIdentifier_provider = Lens.lens (\ActionTypeIdentifier' {provider} -> provider) (\s@ActionTypeIdentifier' {} a -> s {provider = a} :: ActionTypeIdentifier)

-- | A string that describes the action type version.
actionTypeIdentifier_version :: Lens.Lens' ActionTypeIdentifier Prelude.Text
actionTypeIdentifier_version = Lens.lens (\ActionTypeIdentifier' {version} -> version) (\s@ActionTypeIdentifier' {} a -> s {version = a} :: ActionTypeIdentifier)

instance Core.FromJSON ActionTypeIdentifier where
  parseJSON =
    Core.withObject
      "ActionTypeIdentifier"
      ( \x ->
          ActionTypeIdentifier'
            Prelude.<$> (x Core..: "category")
            Prelude.<*> (x Core..: "owner")
            Prelude.<*> (x Core..: "provider")
            Prelude.<*> (x Core..: "version")
      )

instance Prelude.Hashable ActionTypeIdentifier where
  hashWithSalt _salt ActionTypeIdentifier' {..} =
    _salt `Prelude.hashWithSalt` category
      `Prelude.hashWithSalt` owner
      `Prelude.hashWithSalt` provider
      `Prelude.hashWithSalt` version

instance Prelude.NFData ActionTypeIdentifier where
  rnf ActionTypeIdentifier' {..} =
    Prelude.rnf category
      `Prelude.seq` Prelude.rnf owner
      `Prelude.seq` Prelude.rnf provider
      `Prelude.seq` Prelude.rnf version

instance Core.ToJSON ActionTypeIdentifier where
  toJSON ActionTypeIdentifier' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("category" Core..= category),
            Prelude.Just ("owner" Core..= owner),
            Prelude.Just ("provider" Core..= provider),
            Prelude.Just ("version" Core..= version)
          ]
      )

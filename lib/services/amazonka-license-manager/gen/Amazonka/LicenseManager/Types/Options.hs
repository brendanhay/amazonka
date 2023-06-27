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
-- Module      : Amazonka.LicenseManager.Types.Options
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LicenseManager.Types.Options where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LicenseManager.Types.ActivationOverrideBehavior
import qualified Amazonka.Prelude as Prelude

-- | The options you can specify when you create a new version of a grant,
-- such as activation override behavior. For more information, see
-- <https://docs.aws.amazon.com/license-manager/latest/userguide/granted-licenses.html Granted licenses in License Manager>
-- in the /License Manager User Guide/.
--
-- /See:/ 'newOptions' smart constructor.
data Options = Options'
  { -- | An activation option for your grant that determines the behavior of
    -- activating a grant. Activation options can only be used with granted
    -- licenses sourced from the Amazon Web Services Marketplace. Additionally,
    -- the operation must specify the value of @ACTIVE@ for the @Status@
    -- parameter.
    --
    -- -   As a license administrator, you can optionally specify an
    --     @ActivationOverrideBehavior@ when activating a grant.
    --
    -- -   As a grantor, you can optionally specify an
    --     @ActivationOverrideBehavior@ when you activate a grant for a grantee
    --     account in your organization.
    --
    -- -   As a grantee, if the grantor creating the distributed grant doesn’t
    --     specify an @ActivationOverrideBehavior@, you can optionally specify
    --     one when you are activating the grant.
    --
    -- [DISTRIBUTED_GRANTS_ONLY]
    --     Use this value to activate a grant without replacing any member
    --     account’s active grants for the same product.
    --
    -- [ALL_GRANTS_PERMITTED_BY_ISSUER]
    --     Use this value to activate a grant and disable other active grants
    --     in any member accounts for the same product. This action will also
    --     replace their previously activated grants with this activated grant.
    activationOverrideBehavior :: Prelude.Maybe ActivationOverrideBehavior
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Options' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'activationOverrideBehavior', 'options_activationOverrideBehavior' - An activation option for your grant that determines the behavior of
-- activating a grant. Activation options can only be used with granted
-- licenses sourced from the Amazon Web Services Marketplace. Additionally,
-- the operation must specify the value of @ACTIVE@ for the @Status@
-- parameter.
--
-- -   As a license administrator, you can optionally specify an
--     @ActivationOverrideBehavior@ when activating a grant.
--
-- -   As a grantor, you can optionally specify an
--     @ActivationOverrideBehavior@ when you activate a grant for a grantee
--     account in your organization.
--
-- -   As a grantee, if the grantor creating the distributed grant doesn’t
--     specify an @ActivationOverrideBehavior@, you can optionally specify
--     one when you are activating the grant.
--
-- [DISTRIBUTED_GRANTS_ONLY]
--     Use this value to activate a grant without replacing any member
--     account’s active grants for the same product.
--
-- [ALL_GRANTS_PERMITTED_BY_ISSUER]
--     Use this value to activate a grant and disable other active grants
--     in any member accounts for the same product. This action will also
--     replace their previously activated grants with this activated grant.
newOptions ::
  Options
newOptions =
  Options'
    { activationOverrideBehavior =
        Prelude.Nothing
    }

-- | An activation option for your grant that determines the behavior of
-- activating a grant. Activation options can only be used with granted
-- licenses sourced from the Amazon Web Services Marketplace. Additionally,
-- the operation must specify the value of @ACTIVE@ for the @Status@
-- parameter.
--
-- -   As a license administrator, you can optionally specify an
--     @ActivationOverrideBehavior@ when activating a grant.
--
-- -   As a grantor, you can optionally specify an
--     @ActivationOverrideBehavior@ when you activate a grant for a grantee
--     account in your organization.
--
-- -   As a grantee, if the grantor creating the distributed grant doesn’t
--     specify an @ActivationOverrideBehavior@, you can optionally specify
--     one when you are activating the grant.
--
-- [DISTRIBUTED_GRANTS_ONLY]
--     Use this value to activate a grant without replacing any member
--     account’s active grants for the same product.
--
-- [ALL_GRANTS_PERMITTED_BY_ISSUER]
--     Use this value to activate a grant and disable other active grants
--     in any member accounts for the same product. This action will also
--     replace their previously activated grants with this activated grant.
options_activationOverrideBehavior :: Lens.Lens' Options (Prelude.Maybe ActivationOverrideBehavior)
options_activationOverrideBehavior = Lens.lens (\Options' {activationOverrideBehavior} -> activationOverrideBehavior) (\s@Options' {} a -> s {activationOverrideBehavior = a} :: Options)

instance Data.FromJSON Options where
  parseJSON =
    Data.withObject
      "Options"
      ( \x ->
          Options'
            Prelude.<$> (x Data..:? "ActivationOverrideBehavior")
      )

instance Prelude.Hashable Options where
  hashWithSalt _salt Options' {..} =
    _salt
      `Prelude.hashWithSalt` activationOverrideBehavior

instance Prelude.NFData Options where
  rnf Options' {..} =
    Prelude.rnf activationOverrideBehavior

instance Data.ToJSON Options where
  toJSON Options' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ActivationOverrideBehavior" Data..=)
              Prelude.<$> activationOverrideBehavior
          ]
      )

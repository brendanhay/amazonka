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
-- Module      : Amazonka.EKS.Types.IdentityProviderConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EKS.Types.IdentityProviderConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object representing an identity provider configuration.
--
-- /See:/ 'newIdentityProviderConfig' smart constructor.
data IdentityProviderConfig = IdentityProviderConfig'
  { -- | The type of the identity provider configuration. The only type available
    -- is @oidc@.
    type' :: Prelude.Text,
    -- | The name of the identity provider configuration.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IdentityProviderConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'identityProviderConfig_type' - The type of the identity provider configuration. The only type available
-- is @oidc@.
--
-- 'name', 'identityProviderConfig_name' - The name of the identity provider configuration.
newIdentityProviderConfig ::
  -- | 'type''
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  IdentityProviderConfig
newIdentityProviderConfig pType_ pName_ =
  IdentityProviderConfig'
    { type' = pType_,
      name = pName_
    }

-- | The type of the identity provider configuration. The only type available
-- is @oidc@.
identityProviderConfig_type :: Lens.Lens' IdentityProviderConfig Prelude.Text
identityProviderConfig_type = Lens.lens (\IdentityProviderConfig' {type'} -> type') (\s@IdentityProviderConfig' {} a -> s {type' = a} :: IdentityProviderConfig)

-- | The name of the identity provider configuration.
identityProviderConfig_name :: Lens.Lens' IdentityProviderConfig Prelude.Text
identityProviderConfig_name = Lens.lens (\IdentityProviderConfig' {name} -> name) (\s@IdentityProviderConfig' {} a -> s {name = a} :: IdentityProviderConfig)

instance Data.FromJSON IdentityProviderConfig where
  parseJSON =
    Data.withObject
      "IdentityProviderConfig"
      ( \x ->
          IdentityProviderConfig'
            Prelude.<$> (x Data..: "type")
            Prelude.<*> (x Data..: "name")
      )

instance Prelude.Hashable IdentityProviderConfig where
  hashWithSalt _salt IdentityProviderConfig' {..} =
    _salt
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` name

instance Prelude.NFData IdentityProviderConfig where
  rnf IdentityProviderConfig' {..} =
    Prelude.rnf type' `Prelude.seq` Prelude.rnf name

instance Data.ToJSON IdentityProviderConfig where
  toJSON IdentityProviderConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("type" Data..= type'),
            Prelude.Just ("name" Data..= name)
          ]
      )

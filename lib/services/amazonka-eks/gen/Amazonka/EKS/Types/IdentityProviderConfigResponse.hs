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
-- Module      : Amazonka.EKS.Types.IdentityProviderConfigResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EKS.Types.IdentityProviderConfigResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EKS.Types.OidcIdentityProviderConfig
import qualified Amazonka.Prelude as Prelude

-- | The full description of your identity configuration.
--
-- /See:/ 'newIdentityProviderConfigResponse' smart constructor.
data IdentityProviderConfigResponse = IdentityProviderConfigResponse'
  { -- | An object representing an OpenID Connect (OIDC) identity provider
    -- configuration.
    oidc :: Prelude.Maybe OidcIdentityProviderConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IdentityProviderConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'oidc', 'identityProviderConfigResponse_oidc' - An object representing an OpenID Connect (OIDC) identity provider
-- configuration.
newIdentityProviderConfigResponse ::
  IdentityProviderConfigResponse
newIdentityProviderConfigResponse =
  IdentityProviderConfigResponse'
    { oidc =
        Prelude.Nothing
    }

-- | An object representing an OpenID Connect (OIDC) identity provider
-- configuration.
identityProviderConfigResponse_oidc :: Lens.Lens' IdentityProviderConfigResponse (Prelude.Maybe OidcIdentityProviderConfig)
identityProviderConfigResponse_oidc = Lens.lens (\IdentityProviderConfigResponse' {oidc} -> oidc) (\s@IdentityProviderConfigResponse' {} a -> s {oidc = a} :: IdentityProviderConfigResponse)

instance Data.FromJSON IdentityProviderConfigResponse where
  parseJSON =
    Data.withObject
      "IdentityProviderConfigResponse"
      ( \x ->
          IdentityProviderConfigResponse'
            Prelude.<$> (x Data..:? "oidc")
      )

instance
  Prelude.Hashable
    IdentityProviderConfigResponse
  where
  hashWithSalt
    _salt
    IdentityProviderConfigResponse' {..} =
      _salt `Prelude.hashWithSalt` oidc

instance
  Prelude.NFData
    IdentityProviderConfigResponse
  where
  rnf IdentityProviderConfigResponse' {..} =
    Prelude.rnf oidc

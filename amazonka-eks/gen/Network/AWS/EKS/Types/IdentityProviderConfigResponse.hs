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
-- Module      : Network.AWS.EKS.Types.IdentityProviderConfigResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EKS.Types.IdentityProviderConfigResponse where

import qualified Network.AWS.Core as Core
import Network.AWS.EKS.Types.OidcIdentityProviderConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The full description of your identity configuration.
--
-- /See:/ 'newIdentityProviderConfigResponse' smart constructor.
data IdentityProviderConfigResponse = IdentityProviderConfigResponse'
  { -- | An object that represents an OpenID Connect (OIDC) identity provider
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
-- 'oidc', 'identityProviderConfigResponse_oidc' - An object that represents an OpenID Connect (OIDC) identity provider
-- configuration.
newIdentityProviderConfigResponse ::
  IdentityProviderConfigResponse
newIdentityProviderConfigResponse =
  IdentityProviderConfigResponse'
    { oidc =
        Prelude.Nothing
    }

-- | An object that represents an OpenID Connect (OIDC) identity provider
-- configuration.
identityProviderConfigResponse_oidc :: Lens.Lens' IdentityProviderConfigResponse (Prelude.Maybe OidcIdentityProviderConfig)
identityProviderConfigResponse_oidc = Lens.lens (\IdentityProviderConfigResponse' {oidc} -> oidc) (\s@IdentityProviderConfigResponse' {} a -> s {oidc = a} :: IdentityProviderConfigResponse)

instance Core.FromJSON IdentityProviderConfigResponse where
  parseJSON =
    Core.withObject
      "IdentityProviderConfigResponse"
      ( \x ->
          IdentityProviderConfigResponse'
            Prelude.<$> (x Core..:? "oidc")
      )

instance
  Prelude.Hashable
    IdentityProviderConfigResponse

instance
  Prelude.NFData
    IdentityProviderConfigResponse

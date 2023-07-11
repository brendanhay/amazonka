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
-- Module      : Amazonka.EC2.Types.ModifyVerifiedAccessTrustProviderOidcOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.ModifyVerifiedAccessTrustProviderOidcOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | OpenID Connect options for an @oidc@-type, user-identity based trust
-- provider.
--
-- /See:/ 'newModifyVerifiedAccessTrustProviderOidcOptions' smart constructor.
data ModifyVerifiedAccessTrustProviderOidcOptions = ModifyVerifiedAccessTrustProviderOidcOptions'
  { -- | OpenID Connect (OIDC) scopes are used by an application during
    -- authentication to authorize access to a user\'s details. Each scope
    -- returns a specific set of user attributes.
    scope :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyVerifiedAccessTrustProviderOidcOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'scope', 'modifyVerifiedAccessTrustProviderOidcOptions_scope' - OpenID Connect (OIDC) scopes are used by an application during
-- authentication to authorize access to a user\'s details. Each scope
-- returns a specific set of user attributes.
newModifyVerifiedAccessTrustProviderOidcOptions ::
  ModifyVerifiedAccessTrustProviderOidcOptions
newModifyVerifiedAccessTrustProviderOidcOptions =
  ModifyVerifiedAccessTrustProviderOidcOptions'
    { scope =
        Prelude.Nothing
    }

-- | OpenID Connect (OIDC) scopes are used by an application during
-- authentication to authorize access to a user\'s details. Each scope
-- returns a specific set of user attributes.
modifyVerifiedAccessTrustProviderOidcOptions_scope :: Lens.Lens' ModifyVerifiedAccessTrustProviderOidcOptions (Prelude.Maybe Prelude.Text)
modifyVerifiedAccessTrustProviderOidcOptions_scope = Lens.lens (\ModifyVerifiedAccessTrustProviderOidcOptions' {scope} -> scope) (\s@ModifyVerifiedAccessTrustProviderOidcOptions' {} a -> s {scope = a} :: ModifyVerifiedAccessTrustProviderOidcOptions)

instance
  Prelude.Hashable
    ModifyVerifiedAccessTrustProviderOidcOptions
  where
  hashWithSalt
    _salt
    ModifyVerifiedAccessTrustProviderOidcOptions' {..} =
      _salt `Prelude.hashWithSalt` scope

instance
  Prelude.NFData
    ModifyVerifiedAccessTrustProviderOidcOptions
  where
  rnf ModifyVerifiedAccessTrustProviderOidcOptions' {..} =
    Prelude.rnf scope

instance
  Data.ToQuery
    ModifyVerifiedAccessTrustProviderOidcOptions
  where
  toQuery
    ModifyVerifiedAccessTrustProviderOidcOptions' {..} =
      Prelude.mconcat ["Scope" Data.=: scope]

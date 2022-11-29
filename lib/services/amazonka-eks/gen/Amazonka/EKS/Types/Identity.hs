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
-- Module      : Amazonka.EKS.Types.Identity
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EKS.Types.Identity where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EKS.Types.OIDC
import qualified Amazonka.Prelude as Prelude

-- | An object representing an identity provider.
--
-- /See:/ 'newIdentity' smart constructor.
data Identity = Identity'
  { -- | An object representing the <https://openid.net/connect/ OpenID Connect>
    -- identity provider information.
    oidc :: Prelude.Maybe OIDC
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Identity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'oidc', 'identity_oidc' - An object representing the <https://openid.net/connect/ OpenID Connect>
-- identity provider information.
newIdentity ::
  Identity
newIdentity = Identity' {oidc = Prelude.Nothing}

-- | An object representing the <https://openid.net/connect/ OpenID Connect>
-- identity provider information.
identity_oidc :: Lens.Lens' Identity (Prelude.Maybe OIDC)
identity_oidc = Lens.lens (\Identity' {oidc} -> oidc) (\s@Identity' {} a -> s {oidc = a} :: Identity)

instance Core.FromJSON Identity where
  parseJSON =
    Core.withObject
      "Identity"
      (\x -> Identity' Prelude.<$> (x Core..:? "oidc"))

instance Prelude.Hashable Identity where
  hashWithSalt _salt Identity' {..} =
    _salt `Prelude.hashWithSalt` oidc

instance Prelude.NFData Identity where
  rnf Identity' {..} = Prelude.rnf oidc

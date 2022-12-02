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
-- Module      : Amazonka.EKS.Types.OIDC
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EKS.Types.OIDC where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object representing the <https://openid.net/connect/ OpenID Connect>
-- (OIDC) identity provider information for the cluster.
--
-- /See:/ 'newOIDC' smart constructor.
data OIDC = OIDC'
  { -- | The issuer URL for the OIDC identity provider.
    issuer :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OIDC' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'issuer', 'oidc_issuer' - The issuer URL for the OIDC identity provider.
newOIDC ::
  OIDC
newOIDC = OIDC' {issuer = Prelude.Nothing}

-- | The issuer URL for the OIDC identity provider.
oidc_issuer :: Lens.Lens' OIDC (Prelude.Maybe Prelude.Text)
oidc_issuer = Lens.lens (\OIDC' {issuer} -> issuer) (\s@OIDC' {} a -> s {issuer = a} :: OIDC)

instance Data.FromJSON OIDC where
  parseJSON =
    Data.withObject
      "OIDC"
      (\x -> OIDC' Prelude.<$> (x Data..:? "issuer"))

instance Prelude.Hashable OIDC where
  hashWithSalt _salt OIDC' {..} =
    _salt `Prelude.hashWithSalt` issuer

instance Prelude.NFData OIDC where
  rnf OIDC' {..} = Prelude.rnf issuer

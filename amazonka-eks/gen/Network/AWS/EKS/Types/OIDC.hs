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
-- Module      : Network.AWS.EKS.Types.OIDC
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EKS.Types.OIDC where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | An object representing the <https://openid.net/connect/ OpenID Connect>
-- (OIDC) identity provider information for the cluster.
--
-- /See:/ 'newOIDC' smart constructor.
data OIDC = OIDC'
  { -- | The issuer URL for the OIDC identity provider.
    issuer :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
newOIDC = OIDC' {issuer = Core.Nothing}

-- | The issuer URL for the OIDC identity provider.
oidc_issuer :: Lens.Lens' OIDC (Core.Maybe Core.Text)
oidc_issuer = Lens.lens (\OIDC' {issuer} -> issuer) (\s@OIDC' {} a -> s {issuer = a} :: OIDC)

instance Core.FromJSON OIDC where
  parseJSON =
    Core.withObject
      "OIDC"
      (\x -> OIDC' Core.<$> (x Core..:? "issuer"))

instance Core.Hashable OIDC

instance Core.NFData OIDC

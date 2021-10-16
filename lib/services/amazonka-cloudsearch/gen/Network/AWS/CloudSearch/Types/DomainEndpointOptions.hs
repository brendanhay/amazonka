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
-- Module      : Network.AWS.CloudSearch.Types.DomainEndpointOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.DomainEndpointOptions where

import Network.AWS.CloudSearch.Types.TLSSecurityPolicy
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The domain\'s endpoint options.
--
-- /See:/ 'newDomainEndpointOptions' smart constructor.
data DomainEndpointOptions = DomainEndpointOptions'
  { -- | The minimum required TLS version
    tLSSecurityPolicy :: Prelude.Maybe TLSSecurityPolicy,
    -- | Whether the domain is HTTPS only enabled.
    enforceHTTPS :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DomainEndpointOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tLSSecurityPolicy', 'domainEndpointOptions_tLSSecurityPolicy' - The minimum required TLS version
--
-- 'enforceHTTPS', 'domainEndpointOptions_enforceHTTPS' - Whether the domain is HTTPS only enabled.
newDomainEndpointOptions ::
  DomainEndpointOptions
newDomainEndpointOptions =
  DomainEndpointOptions'
    { tLSSecurityPolicy =
        Prelude.Nothing,
      enforceHTTPS = Prelude.Nothing
    }

-- | The minimum required TLS version
domainEndpointOptions_tLSSecurityPolicy :: Lens.Lens' DomainEndpointOptions (Prelude.Maybe TLSSecurityPolicy)
domainEndpointOptions_tLSSecurityPolicy = Lens.lens (\DomainEndpointOptions' {tLSSecurityPolicy} -> tLSSecurityPolicy) (\s@DomainEndpointOptions' {} a -> s {tLSSecurityPolicy = a} :: DomainEndpointOptions)

-- | Whether the domain is HTTPS only enabled.
domainEndpointOptions_enforceHTTPS :: Lens.Lens' DomainEndpointOptions (Prelude.Maybe Prelude.Bool)
domainEndpointOptions_enforceHTTPS = Lens.lens (\DomainEndpointOptions' {enforceHTTPS} -> enforceHTTPS) (\s@DomainEndpointOptions' {} a -> s {enforceHTTPS = a} :: DomainEndpointOptions)

instance Core.FromXML DomainEndpointOptions where
  parseXML x =
    DomainEndpointOptions'
      Prelude.<$> (x Core..@? "TLSSecurityPolicy")
      Prelude.<*> (x Core..@? "EnforceHTTPS")

instance Prelude.Hashable DomainEndpointOptions

instance Prelude.NFData DomainEndpointOptions

instance Core.ToQuery DomainEndpointOptions where
  toQuery DomainEndpointOptions' {..} =
    Prelude.mconcat
      [ "TLSSecurityPolicy" Core.=: tLSSecurityPolicy,
        "EnforceHTTPS" Core.=: enforceHTTPS
      ]

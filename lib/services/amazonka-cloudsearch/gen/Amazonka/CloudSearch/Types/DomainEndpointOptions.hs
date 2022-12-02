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
-- Module      : Amazonka.CloudSearch.Types.DomainEndpointOptions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudSearch.Types.DomainEndpointOptions where

import Amazonka.CloudSearch.Types.TLSSecurityPolicy
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

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

instance Data.FromXML DomainEndpointOptions where
  parseXML x =
    DomainEndpointOptions'
      Prelude.<$> (x Data..@? "TLSSecurityPolicy")
      Prelude.<*> (x Data..@? "EnforceHTTPS")

instance Prelude.Hashable DomainEndpointOptions where
  hashWithSalt _salt DomainEndpointOptions' {..} =
    _salt `Prelude.hashWithSalt` tLSSecurityPolicy
      `Prelude.hashWithSalt` enforceHTTPS

instance Prelude.NFData DomainEndpointOptions where
  rnf DomainEndpointOptions' {..} =
    Prelude.rnf tLSSecurityPolicy
      `Prelude.seq` Prelude.rnf enforceHTTPS

instance Data.ToQuery DomainEndpointOptions where
  toQuery DomainEndpointOptions' {..} =
    Prelude.mconcat
      [ "TLSSecurityPolicy" Data.=: tLSSecurityPolicy,
        "EnforceHTTPS" Data.=: enforceHTTPS
      ]

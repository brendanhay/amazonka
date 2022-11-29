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
-- Module      : Amazonka.SecurityHub.Types.DnsRequestAction
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.DnsRequestAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provided if @ActionType@ is @DNS_REQUEST@. It provides details about the
-- DNS request that was detected.
--
-- /See:/ 'newDnsRequestAction' smart constructor.
data DnsRequestAction = DnsRequestAction'
  { -- | The DNS domain that is associated with the DNS request.
    domain :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the DNS request was blocked.
    blocked :: Prelude.Maybe Prelude.Bool,
    -- | The protocol that was used for the DNS request.
    protocol :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DnsRequestAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domain', 'dnsRequestAction_domain' - The DNS domain that is associated with the DNS request.
--
-- 'blocked', 'dnsRequestAction_blocked' - Indicates whether the DNS request was blocked.
--
-- 'protocol', 'dnsRequestAction_protocol' - The protocol that was used for the DNS request.
newDnsRequestAction ::
  DnsRequestAction
newDnsRequestAction =
  DnsRequestAction'
    { domain = Prelude.Nothing,
      blocked = Prelude.Nothing,
      protocol = Prelude.Nothing
    }

-- | The DNS domain that is associated with the DNS request.
dnsRequestAction_domain :: Lens.Lens' DnsRequestAction (Prelude.Maybe Prelude.Text)
dnsRequestAction_domain = Lens.lens (\DnsRequestAction' {domain} -> domain) (\s@DnsRequestAction' {} a -> s {domain = a} :: DnsRequestAction)

-- | Indicates whether the DNS request was blocked.
dnsRequestAction_blocked :: Lens.Lens' DnsRequestAction (Prelude.Maybe Prelude.Bool)
dnsRequestAction_blocked = Lens.lens (\DnsRequestAction' {blocked} -> blocked) (\s@DnsRequestAction' {} a -> s {blocked = a} :: DnsRequestAction)

-- | The protocol that was used for the DNS request.
dnsRequestAction_protocol :: Lens.Lens' DnsRequestAction (Prelude.Maybe Prelude.Text)
dnsRequestAction_protocol = Lens.lens (\DnsRequestAction' {protocol} -> protocol) (\s@DnsRequestAction' {} a -> s {protocol = a} :: DnsRequestAction)

instance Core.FromJSON DnsRequestAction where
  parseJSON =
    Core.withObject
      "DnsRequestAction"
      ( \x ->
          DnsRequestAction'
            Prelude.<$> (x Core..:? "Domain")
            Prelude.<*> (x Core..:? "Blocked")
            Prelude.<*> (x Core..:? "Protocol")
      )

instance Prelude.Hashable DnsRequestAction where
  hashWithSalt _salt DnsRequestAction' {..} =
    _salt `Prelude.hashWithSalt` domain
      `Prelude.hashWithSalt` blocked
      `Prelude.hashWithSalt` protocol

instance Prelude.NFData DnsRequestAction where
  rnf DnsRequestAction' {..} =
    Prelude.rnf domain
      `Prelude.seq` Prelude.rnf blocked
      `Prelude.seq` Prelude.rnf protocol

instance Core.ToJSON DnsRequestAction where
  toJSON DnsRequestAction' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Domain" Core..=) Prelude.<$> domain,
            ("Blocked" Core..=) Prelude.<$> blocked,
            ("Protocol" Core..=) Prelude.<$> protocol
          ]
      )

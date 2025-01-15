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
-- Module      : Amazonka.GuardDuty.Types.DnsRequestAction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.DnsRequestAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the DNS_REQUEST action described in this
-- finding.
--
-- /See:/ 'newDnsRequestAction' smart constructor.
data DnsRequestAction = DnsRequestAction'
  { -- | Indicates whether the targeted port is blocked.
    blocked :: Prelude.Maybe Prelude.Bool,
    -- | The domain information for the API request.
    domain :: Prelude.Maybe Prelude.Text,
    -- | The network connection protocol observed in the activity that prompted
    -- GuardDuty to generate the finding.
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
-- 'blocked', 'dnsRequestAction_blocked' - Indicates whether the targeted port is blocked.
--
-- 'domain', 'dnsRequestAction_domain' - The domain information for the API request.
--
-- 'protocol', 'dnsRequestAction_protocol' - The network connection protocol observed in the activity that prompted
-- GuardDuty to generate the finding.
newDnsRequestAction ::
  DnsRequestAction
newDnsRequestAction =
  DnsRequestAction'
    { blocked = Prelude.Nothing,
      domain = Prelude.Nothing,
      protocol = Prelude.Nothing
    }

-- | Indicates whether the targeted port is blocked.
dnsRequestAction_blocked :: Lens.Lens' DnsRequestAction (Prelude.Maybe Prelude.Bool)
dnsRequestAction_blocked = Lens.lens (\DnsRequestAction' {blocked} -> blocked) (\s@DnsRequestAction' {} a -> s {blocked = a} :: DnsRequestAction)

-- | The domain information for the API request.
dnsRequestAction_domain :: Lens.Lens' DnsRequestAction (Prelude.Maybe Prelude.Text)
dnsRequestAction_domain = Lens.lens (\DnsRequestAction' {domain} -> domain) (\s@DnsRequestAction' {} a -> s {domain = a} :: DnsRequestAction)

-- | The network connection protocol observed in the activity that prompted
-- GuardDuty to generate the finding.
dnsRequestAction_protocol :: Lens.Lens' DnsRequestAction (Prelude.Maybe Prelude.Text)
dnsRequestAction_protocol = Lens.lens (\DnsRequestAction' {protocol} -> protocol) (\s@DnsRequestAction' {} a -> s {protocol = a} :: DnsRequestAction)

instance Data.FromJSON DnsRequestAction where
  parseJSON =
    Data.withObject
      "DnsRequestAction"
      ( \x ->
          DnsRequestAction'
            Prelude.<$> (x Data..:? "blocked")
            Prelude.<*> (x Data..:? "domain")
            Prelude.<*> (x Data..:? "protocol")
      )

instance Prelude.Hashable DnsRequestAction where
  hashWithSalt _salt DnsRequestAction' {..} =
    _salt
      `Prelude.hashWithSalt` blocked
      `Prelude.hashWithSalt` domain
      `Prelude.hashWithSalt` protocol

instance Prelude.NFData DnsRequestAction where
  rnf DnsRequestAction' {..} =
    Prelude.rnf blocked `Prelude.seq`
      Prelude.rnf domain `Prelude.seq`
        Prelude.rnf protocol

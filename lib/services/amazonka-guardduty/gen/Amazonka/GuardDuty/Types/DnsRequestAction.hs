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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.DnsRequestAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the DNS_REQUEST action described in this
-- finding.
--
-- /See:/ 'newDnsRequestAction' smart constructor.
data DnsRequestAction = DnsRequestAction'
  { -- | The domain information for the API request.
    domain :: Prelude.Maybe Prelude.Text
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
-- 'domain', 'dnsRequestAction_domain' - The domain information for the API request.
newDnsRequestAction ::
  DnsRequestAction
newDnsRequestAction =
  DnsRequestAction' {domain = Prelude.Nothing}

-- | The domain information for the API request.
dnsRequestAction_domain :: Lens.Lens' DnsRequestAction (Prelude.Maybe Prelude.Text)
dnsRequestAction_domain = Lens.lens (\DnsRequestAction' {domain} -> domain) (\s@DnsRequestAction' {} a -> s {domain = a} :: DnsRequestAction)

instance Core.FromJSON DnsRequestAction where
  parseJSON =
    Core.withObject
      "DnsRequestAction"
      ( \x ->
          DnsRequestAction' Prelude.<$> (x Core..:? "domain")
      )

instance Prelude.Hashable DnsRequestAction where
  hashWithSalt _salt DnsRequestAction' {..} =
    _salt `Prelude.hashWithSalt` domain

instance Prelude.NFData DnsRequestAction where
  rnf DnsRequestAction' {..} = Prelude.rnf domain

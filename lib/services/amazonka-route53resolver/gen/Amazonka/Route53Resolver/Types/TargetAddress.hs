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
-- Module      : Amazonka.Route53Resolver.Types.TargetAddress
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53Resolver.Types.TargetAddress where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | In a
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_route53resolver_CreateResolverRule.html CreateResolverRule>
-- request, an array of the IPs that you want to forward DNS queries to.
--
-- /See:/ 'newTargetAddress' smart constructor.
data TargetAddress = TargetAddress'
  { -- | One IPv4 address that you want to forward DNS queries to.
    ip :: Prelude.Maybe Prelude.Text,
    -- | One IPv6 address that you want to forward DNS queries to.
    ipv6 :: Prelude.Maybe Prelude.Text,
    -- | The port at @Ip@ that you want to forward DNS queries to.
    port :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TargetAddress' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ip', 'targetAddress_ip' - One IPv4 address that you want to forward DNS queries to.
--
-- 'ipv6', 'targetAddress_ipv6' - One IPv6 address that you want to forward DNS queries to.
--
-- 'port', 'targetAddress_port' - The port at @Ip@ that you want to forward DNS queries to.
newTargetAddress ::
  TargetAddress
newTargetAddress =
  TargetAddress'
    { ip = Prelude.Nothing,
      ipv6 = Prelude.Nothing,
      port = Prelude.Nothing
    }

-- | One IPv4 address that you want to forward DNS queries to.
targetAddress_ip :: Lens.Lens' TargetAddress (Prelude.Maybe Prelude.Text)
targetAddress_ip = Lens.lens (\TargetAddress' {ip} -> ip) (\s@TargetAddress' {} a -> s {ip = a} :: TargetAddress)

-- | One IPv6 address that you want to forward DNS queries to.
targetAddress_ipv6 :: Lens.Lens' TargetAddress (Prelude.Maybe Prelude.Text)
targetAddress_ipv6 = Lens.lens (\TargetAddress' {ipv6} -> ipv6) (\s@TargetAddress' {} a -> s {ipv6 = a} :: TargetAddress)

-- | The port at @Ip@ that you want to forward DNS queries to.
targetAddress_port :: Lens.Lens' TargetAddress (Prelude.Maybe Prelude.Natural)
targetAddress_port = Lens.lens (\TargetAddress' {port} -> port) (\s@TargetAddress' {} a -> s {port = a} :: TargetAddress)

instance Data.FromJSON TargetAddress where
  parseJSON =
    Data.withObject
      "TargetAddress"
      ( \x ->
          TargetAddress'
            Prelude.<$> (x Data..:? "Ip")
            Prelude.<*> (x Data..:? "Ipv6")
            Prelude.<*> (x Data..:? "Port")
      )

instance Prelude.Hashable TargetAddress where
  hashWithSalt _salt TargetAddress' {..} =
    _salt
      `Prelude.hashWithSalt` ip
      `Prelude.hashWithSalt` ipv6
      `Prelude.hashWithSalt` port

instance Prelude.NFData TargetAddress where
  rnf TargetAddress' {..} =
    Prelude.rnf ip
      `Prelude.seq` Prelude.rnf ipv6
      `Prelude.seq` Prelude.rnf port

instance Data.ToJSON TargetAddress where
  toJSON TargetAddress' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Ip" Data..=) Prelude.<$> ip,
            ("Ipv6" Data..=) Prelude.<$> ipv6,
            ("Port" Data..=) Prelude.<$> port
          ]
      )

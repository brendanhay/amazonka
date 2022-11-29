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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53Resolver.Types.TargetAddress where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | In a
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_route53resolver_CreateResolverRule.html CreateResolverRule>
-- request, an array of the IPs that you want to forward DNS queries to.
--
-- /See:/ 'newTargetAddress' smart constructor.
data TargetAddress = TargetAddress'
  { -- | The port at @Ip@ that you want to forward DNS queries to.
    port :: Prelude.Maybe Prelude.Natural,
    -- | One IP address that you want to forward DNS queries to. You can specify
    -- only IPv4 addresses.
    ip :: Prelude.Text
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
-- 'port', 'targetAddress_port' - The port at @Ip@ that you want to forward DNS queries to.
--
-- 'ip', 'targetAddress_ip' - One IP address that you want to forward DNS queries to. You can specify
-- only IPv4 addresses.
newTargetAddress ::
  -- | 'ip'
  Prelude.Text ->
  TargetAddress
newTargetAddress pIp_ =
  TargetAddress' {port = Prelude.Nothing, ip = pIp_}

-- | The port at @Ip@ that you want to forward DNS queries to.
targetAddress_port :: Lens.Lens' TargetAddress (Prelude.Maybe Prelude.Natural)
targetAddress_port = Lens.lens (\TargetAddress' {port} -> port) (\s@TargetAddress' {} a -> s {port = a} :: TargetAddress)

-- | One IP address that you want to forward DNS queries to. You can specify
-- only IPv4 addresses.
targetAddress_ip :: Lens.Lens' TargetAddress Prelude.Text
targetAddress_ip = Lens.lens (\TargetAddress' {ip} -> ip) (\s@TargetAddress' {} a -> s {ip = a} :: TargetAddress)

instance Core.FromJSON TargetAddress where
  parseJSON =
    Core.withObject
      "TargetAddress"
      ( \x ->
          TargetAddress'
            Prelude.<$> (x Core..:? "Port") Prelude.<*> (x Core..: "Ip")
      )

instance Prelude.Hashable TargetAddress where
  hashWithSalt _salt TargetAddress' {..} =
    _salt `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` ip

instance Prelude.NFData TargetAddress where
  rnf TargetAddress' {..} =
    Prelude.rnf port `Prelude.seq` Prelude.rnf ip

instance Core.ToJSON TargetAddress where
  toJSON TargetAddress' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Port" Core..=) Prelude.<$> port,
            Prelude.Just ("Ip" Core..= ip)
          ]
      )

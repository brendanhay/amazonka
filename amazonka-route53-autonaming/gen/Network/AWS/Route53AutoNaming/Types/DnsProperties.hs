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
-- Module      : Network.AWS.Route53AutoNaming.Types.DnsProperties
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53AutoNaming.Types.DnsProperties where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A complex type that contains the ID for the Route 53 hosted zone that
-- AWS Cloud Map creates when you create a namespace.
--
-- /See:/ 'newDnsProperties' smart constructor.
data DnsProperties = DnsProperties'
  { -- | The ID for the Route 53 hosted zone that AWS Cloud Map creates when you
    -- create a namespace.
    hostedZoneId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DnsProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hostedZoneId', 'dnsProperties_hostedZoneId' - The ID for the Route 53 hosted zone that AWS Cloud Map creates when you
-- create a namespace.
newDnsProperties ::
  DnsProperties
newDnsProperties =
  DnsProperties' {hostedZoneId = Core.Nothing}

-- | The ID for the Route 53 hosted zone that AWS Cloud Map creates when you
-- create a namespace.
dnsProperties_hostedZoneId :: Lens.Lens' DnsProperties (Core.Maybe Core.Text)
dnsProperties_hostedZoneId = Lens.lens (\DnsProperties' {hostedZoneId} -> hostedZoneId) (\s@DnsProperties' {} a -> s {hostedZoneId = a} :: DnsProperties)

instance Core.FromJSON DnsProperties where
  parseJSON =
    Core.withObject
      "DnsProperties"
      ( \x ->
          DnsProperties' Core.<$> (x Core..:? "HostedZoneId")
      )

instance Core.Hashable DnsProperties

instance Core.NFData DnsProperties

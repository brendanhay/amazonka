{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A complex type that contains the ID for the Route 53 hosted zone that
-- AWS Cloud Map creates when you create a namespace.
--
-- /See:/ 'newDnsProperties' smart constructor.
data DnsProperties = DnsProperties'
  { -- | The ID for the Route 53 hosted zone that AWS Cloud Map creates when you
    -- create a namespace.
    hostedZoneId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  DnsProperties' {hostedZoneId = Prelude.Nothing}

-- | The ID for the Route 53 hosted zone that AWS Cloud Map creates when you
-- create a namespace.
dnsProperties_hostedZoneId :: Lens.Lens' DnsProperties (Prelude.Maybe Prelude.Text)
dnsProperties_hostedZoneId = Lens.lens (\DnsProperties' {hostedZoneId} -> hostedZoneId) (\s@DnsProperties' {} a -> s {hostedZoneId = a} :: DnsProperties)

instance Prelude.FromJSON DnsProperties where
  parseJSON =
    Prelude.withObject
      "DnsProperties"
      ( \x ->
          DnsProperties'
            Prelude.<$> (x Prelude..:? "HostedZoneId")
      )

instance Prelude.Hashable DnsProperties

instance Prelude.NFData DnsProperties

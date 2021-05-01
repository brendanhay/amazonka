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
-- Module      : Network.AWS.ECS.Types.NetworkInterface
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.NetworkInterface where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An object representing the elastic network interface for tasks that use
-- the @awsvpc@ network mode.
--
-- /See:/ 'newNetworkInterface' smart constructor.
data NetworkInterface = NetworkInterface'
  { -- | The private IPv4 address for the network interface.
    privateIpv4Address :: Prelude.Maybe Prelude.Text,
    -- | The private IPv6 address for the network interface.
    ipv6Address :: Prelude.Maybe Prelude.Text,
    -- | The attachment ID for the network interface.
    attachmentId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'NetworkInterface' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'privateIpv4Address', 'networkInterface_privateIpv4Address' - The private IPv4 address for the network interface.
--
-- 'ipv6Address', 'networkInterface_ipv6Address' - The private IPv6 address for the network interface.
--
-- 'attachmentId', 'networkInterface_attachmentId' - The attachment ID for the network interface.
newNetworkInterface ::
  NetworkInterface
newNetworkInterface =
  NetworkInterface'
    { privateIpv4Address =
        Prelude.Nothing,
      ipv6Address = Prelude.Nothing,
      attachmentId = Prelude.Nothing
    }

-- | The private IPv4 address for the network interface.
networkInterface_privateIpv4Address :: Lens.Lens' NetworkInterface (Prelude.Maybe Prelude.Text)
networkInterface_privateIpv4Address = Lens.lens (\NetworkInterface' {privateIpv4Address} -> privateIpv4Address) (\s@NetworkInterface' {} a -> s {privateIpv4Address = a} :: NetworkInterface)

-- | The private IPv6 address for the network interface.
networkInterface_ipv6Address :: Lens.Lens' NetworkInterface (Prelude.Maybe Prelude.Text)
networkInterface_ipv6Address = Lens.lens (\NetworkInterface' {ipv6Address} -> ipv6Address) (\s@NetworkInterface' {} a -> s {ipv6Address = a} :: NetworkInterface)

-- | The attachment ID for the network interface.
networkInterface_attachmentId :: Lens.Lens' NetworkInterface (Prelude.Maybe Prelude.Text)
networkInterface_attachmentId = Lens.lens (\NetworkInterface' {attachmentId} -> attachmentId) (\s@NetworkInterface' {} a -> s {attachmentId = a} :: NetworkInterface)

instance Prelude.FromJSON NetworkInterface where
  parseJSON =
    Prelude.withObject
      "NetworkInterface"
      ( \x ->
          NetworkInterface'
            Prelude.<$> (x Prelude..:? "privateIpv4Address")
            Prelude.<*> (x Prelude..:? "ipv6Address")
            Prelude.<*> (x Prelude..:? "attachmentId")
      )

instance Prelude.Hashable NetworkInterface

instance Prelude.NFData NetworkInterface

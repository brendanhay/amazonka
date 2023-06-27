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
-- Module      : Amazonka.Batch.Types.NetworkInterface
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Batch.Types.NetworkInterface where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that represents the elastic network interface for a multi-node
-- parallel job node.
--
-- /See:/ 'newNetworkInterface' smart constructor.
data NetworkInterface = NetworkInterface'
  { -- | The attachment ID for the network interface.
    attachmentId :: Prelude.Maybe Prelude.Text,
    -- | The private IPv6 address for the network interface.
    ipv6Address :: Prelude.Maybe Prelude.Text,
    -- | The private IPv4 address for the network interface.
    privateIpv4Address :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NetworkInterface' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attachmentId', 'networkInterface_attachmentId' - The attachment ID for the network interface.
--
-- 'ipv6Address', 'networkInterface_ipv6Address' - The private IPv6 address for the network interface.
--
-- 'privateIpv4Address', 'networkInterface_privateIpv4Address' - The private IPv4 address for the network interface.
newNetworkInterface ::
  NetworkInterface
newNetworkInterface =
  NetworkInterface'
    { attachmentId = Prelude.Nothing,
      ipv6Address = Prelude.Nothing,
      privateIpv4Address = Prelude.Nothing
    }

-- | The attachment ID for the network interface.
networkInterface_attachmentId :: Lens.Lens' NetworkInterface (Prelude.Maybe Prelude.Text)
networkInterface_attachmentId = Lens.lens (\NetworkInterface' {attachmentId} -> attachmentId) (\s@NetworkInterface' {} a -> s {attachmentId = a} :: NetworkInterface)

-- | The private IPv6 address for the network interface.
networkInterface_ipv6Address :: Lens.Lens' NetworkInterface (Prelude.Maybe Prelude.Text)
networkInterface_ipv6Address = Lens.lens (\NetworkInterface' {ipv6Address} -> ipv6Address) (\s@NetworkInterface' {} a -> s {ipv6Address = a} :: NetworkInterface)

-- | The private IPv4 address for the network interface.
networkInterface_privateIpv4Address :: Lens.Lens' NetworkInterface (Prelude.Maybe Prelude.Text)
networkInterface_privateIpv4Address = Lens.lens (\NetworkInterface' {privateIpv4Address} -> privateIpv4Address) (\s@NetworkInterface' {} a -> s {privateIpv4Address = a} :: NetworkInterface)

instance Data.FromJSON NetworkInterface where
  parseJSON =
    Data.withObject
      "NetworkInterface"
      ( \x ->
          NetworkInterface'
            Prelude.<$> (x Data..:? "attachmentId")
            Prelude.<*> (x Data..:? "ipv6Address")
            Prelude.<*> (x Data..:? "privateIpv4Address")
      )

instance Prelude.Hashable NetworkInterface where
  hashWithSalt _salt NetworkInterface' {..} =
    _salt
      `Prelude.hashWithSalt` attachmentId
      `Prelude.hashWithSalt` ipv6Address
      `Prelude.hashWithSalt` privateIpv4Address

instance Prelude.NFData NetworkInterface where
  rnf NetworkInterface' {..} =
    Prelude.rnf attachmentId
      `Prelude.seq` Prelude.rnf ipv6Address
      `Prelude.seq` Prelude.rnf privateIpv4Address

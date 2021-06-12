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
-- Module      : Network.AWS.EC2.Types.PublicIpv4PoolRange
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.PublicIpv4PoolRange where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens

-- | Describes an address range of an IPv4 address pool.
--
-- /See:/ 'newPublicIpv4PoolRange' smart constructor.
data PublicIpv4PoolRange = PublicIpv4PoolRange'
  { -- | The number of addresses in the range.
    addressCount :: Core.Maybe Core.Int,
    -- | The first IP address in the range.
    firstAddress :: Core.Maybe Core.Text,
    -- | The last IP address in the range.
    lastAddress :: Core.Maybe Core.Text,
    -- | The number of available addresses in the range.
    availableAddressCount :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PublicIpv4PoolRange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'addressCount', 'publicIpv4PoolRange_addressCount' - The number of addresses in the range.
--
-- 'firstAddress', 'publicIpv4PoolRange_firstAddress' - The first IP address in the range.
--
-- 'lastAddress', 'publicIpv4PoolRange_lastAddress' - The last IP address in the range.
--
-- 'availableAddressCount', 'publicIpv4PoolRange_availableAddressCount' - The number of available addresses in the range.
newPublicIpv4PoolRange ::
  PublicIpv4PoolRange
newPublicIpv4PoolRange =
  PublicIpv4PoolRange'
    { addressCount = Core.Nothing,
      firstAddress = Core.Nothing,
      lastAddress = Core.Nothing,
      availableAddressCount = Core.Nothing
    }

-- | The number of addresses in the range.
publicIpv4PoolRange_addressCount :: Lens.Lens' PublicIpv4PoolRange (Core.Maybe Core.Int)
publicIpv4PoolRange_addressCount = Lens.lens (\PublicIpv4PoolRange' {addressCount} -> addressCount) (\s@PublicIpv4PoolRange' {} a -> s {addressCount = a} :: PublicIpv4PoolRange)

-- | The first IP address in the range.
publicIpv4PoolRange_firstAddress :: Lens.Lens' PublicIpv4PoolRange (Core.Maybe Core.Text)
publicIpv4PoolRange_firstAddress = Lens.lens (\PublicIpv4PoolRange' {firstAddress} -> firstAddress) (\s@PublicIpv4PoolRange' {} a -> s {firstAddress = a} :: PublicIpv4PoolRange)

-- | The last IP address in the range.
publicIpv4PoolRange_lastAddress :: Lens.Lens' PublicIpv4PoolRange (Core.Maybe Core.Text)
publicIpv4PoolRange_lastAddress = Lens.lens (\PublicIpv4PoolRange' {lastAddress} -> lastAddress) (\s@PublicIpv4PoolRange' {} a -> s {lastAddress = a} :: PublicIpv4PoolRange)

-- | The number of available addresses in the range.
publicIpv4PoolRange_availableAddressCount :: Lens.Lens' PublicIpv4PoolRange (Core.Maybe Core.Int)
publicIpv4PoolRange_availableAddressCount = Lens.lens (\PublicIpv4PoolRange' {availableAddressCount} -> availableAddressCount) (\s@PublicIpv4PoolRange' {} a -> s {availableAddressCount = a} :: PublicIpv4PoolRange)

instance Core.FromXML PublicIpv4PoolRange where
  parseXML x =
    PublicIpv4PoolRange'
      Core.<$> (x Core..@? "addressCount")
      Core.<*> (x Core..@? "firstAddress")
      Core.<*> (x Core..@? "lastAddress")
      Core.<*> (x Core..@? "availableAddressCount")

instance Core.Hashable PublicIpv4PoolRange

instance Core.NFData PublicIpv4PoolRange

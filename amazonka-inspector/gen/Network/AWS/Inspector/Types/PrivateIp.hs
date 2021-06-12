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
-- Module      : Network.AWS.Inspector.Types.PrivateIp
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.PrivateIp where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains information about a private IP address associated with a
-- network interface. This data type is used as a response element in the
-- DescribeFindings action.
--
-- /See:/ 'newPrivateIp' smart constructor.
data PrivateIp = PrivateIp'
  { -- | The DNS name of the private IP address.
    privateDnsName :: Core.Maybe Core.Text,
    -- | The full IP address of the network inteface.
    privateIpAddress :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PrivateIp' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'privateDnsName', 'privateIp_privateDnsName' - The DNS name of the private IP address.
--
-- 'privateIpAddress', 'privateIp_privateIpAddress' - The full IP address of the network inteface.
newPrivateIp ::
  PrivateIp
newPrivateIp =
  PrivateIp'
    { privateDnsName = Core.Nothing,
      privateIpAddress = Core.Nothing
    }

-- | The DNS name of the private IP address.
privateIp_privateDnsName :: Lens.Lens' PrivateIp (Core.Maybe Core.Text)
privateIp_privateDnsName = Lens.lens (\PrivateIp' {privateDnsName} -> privateDnsName) (\s@PrivateIp' {} a -> s {privateDnsName = a} :: PrivateIp)

-- | The full IP address of the network inteface.
privateIp_privateIpAddress :: Lens.Lens' PrivateIp (Core.Maybe Core.Text)
privateIp_privateIpAddress = Lens.lens (\PrivateIp' {privateIpAddress} -> privateIpAddress) (\s@PrivateIp' {} a -> s {privateIpAddress = a} :: PrivateIp)

instance Core.FromJSON PrivateIp where
  parseJSON =
    Core.withObject
      "PrivateIp"
      ( \x ->
          PrivateIp'
            Core.<$> (x Core..:? "privateDnsName")
            Core.<*> (x Core..:? "privateIpAddress")
      )

instance Core.Hashable PrivateIp

instance Core.NFData PrivateIp

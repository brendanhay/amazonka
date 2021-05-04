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
-- Module      : Network.AWS.Inspector.Types.PrivateIp
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.PrivateIp where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains information about a private IP address associated with a
-- network interface. This data type is used as a response element in the
-- DescribeFindings action.
--
-- /See:/ 'newPrivateIp' smart constructor.
data PrivateIp = PrivateIp'
  { -- | The DNS name of the private IP address.
    privateDnsName :: Prelude.Maybe Prelude.Text,
    -- | The full IP address of the network inteface.
    privateIpAddress :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { privateDnsName = Prelude.Nothing,
      privateIpAddress = Prelude.Nothing
    }

-- | The DNS name of the private IP address.
privateIp_privateDnsName :: Lens.Lens' PrivateIp (Prelude.Maybe Prelude.Text)
privateIp_privateDnsName = Lens.lens (\PrivateIp' {privateDnsName} -> privateDnsName) (\s@PrivateIp' {} a -> s {privateDnsName = a} :: PrivateIp)

-- | The full IP address of the network inteface.
privateIp_privateIpAddress :: Lens.Lens' PrivateIp (Prelude.Maybe Prelude.Text)
privateIp_privateIpAddress = Lens.lens (\PrivateIp' {privateIpAddress} -> privateIpAddress) (\s@PrivateIp' {} a -> s {privateIpAddress = a} :: PrivateIp)

instance Prelude.FromJSON PrivateIp where
  parseJSON =
    Prelude.withObject
      "PrivateIp"
      ( \x ->
          PrivateIp'
            Prelude.<$> (x Prelude..:? "privateDnsName")
            Prelude.<*> (x Prelude..:? "privateIpAddress")
      )

instance Prelude.Hashable PrivateIp

instance Prelude.NFData PrivateIp

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
-- Module      : Amazonka.Inspector.Types.PrivateIp
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector.Types.PrivateIp where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a private IP address associated with a
-- network interface. This data type is used as a response element in the
-- DescribeFindings action.
--
-- /See:/ 'newPrivateIp' smart constructor.
data PrivateIp = PrivateIp'
  { -- | The full IP address of the network inteface.
    privateIpAddress :: Prelude.Maybe Prelude.Text,
    -- | The DNS name of the private IP address.
    privateDnsName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PrivateIp' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'privateIpAddress', 'privateIp_privateIpAddress' - The full IP address of the network inteface.
--
-- 'privateDnsName', 'privateIp_privateDnsName' - The DNS name of the private IP address.
newPrivateIp ::
  PrivateIp
newPrivateIp =
  PrivateIp'
    { privateIpAddress = Prelude.Nothing,
      privateDnsName = Prelude.Nothing
    }

-- | The full IP address of the network inteface.
privateIp_privateIpAddress :: Lens.Lens' PrivateIp (Prelude.Maybe Prelude.Text)
privateIp_privateIpAddress = Lens.lens (\PrivateIp' {privateIpAddress} -> privateIpAddress) (\s@PrivateIp' {} a -> s {privateIpAddress = a} :: PrivateIp)

-- | The DNS name of the private IP address.
privateIp_privateDnsName :: Lens.Lens' PrivateIp (Prelude.Maybe Prelude.Text)
privateIp_privateDnsName = Lens.lens (\PrivateIp' {privateDnsName} -> privateDnsName) (\s@PrivateIp' {} a -> s {privateDnsName = a} :: PrivateIp)

instance Core.FromJSON PrivateIp where
  parseJSON =
    Core.withObject
      "PrivateIp"
      ( \x ->
          PrivateIp'
            Prelude.<$> (x Core..:? "privateIpAddress")
            Prelude.<*> (x Core..:? "privateDnsName")
      )

instance Prelude.Hashable PrivateIp where
  hashWithSalt _salt PrivateIp' {..} =
    _salt `Prelude.hashWithSalt` privateIpAddress
      `Prelude.hashWithSalt` privateDnsName

instance Prelude.NFData PrivateIp where
  rnf PrivateIp' {..} =
    Prelude.rnf privateIpAddress
      `Prelude.seq` Prelude.rnf privateDnsName

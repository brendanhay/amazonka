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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector.Types.PrivateIp where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Data.FromJSON PrivateIp where
  parseJSON =
    Data.withObject
      "PrivateIp"
      ( \x ->
          PrivateIp'
            Prelude.<$> (x Data..:? "privateDnsName")
            Prelude.<*> (x Data..:? "privateIpAddress")
      )

instance Prelude.Hashable PrivateIp where
  hashWithSalt _salt PrivateIp' {..} =
    _salt
      `Prelude.hashWithSalt` privateDnsName
      `Prelude.hashWithSalt` privateIpAddress

instance Prelude.NFData PrivateIp where
  rnf PrivateIp' {..} =
    Prelude.rnf privateDnsName
      `Prelude.seq` Prelude.rnf privateIpAddress

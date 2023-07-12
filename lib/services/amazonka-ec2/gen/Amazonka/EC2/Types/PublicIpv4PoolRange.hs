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
-- Module      : Amazonka.EC2.Types.PublicIpv4PoolRange
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.PublicIpv4PoolRange where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Describes an address range of an IPv4 address pool.
--
-- /See:/ 'newPublicIpv4PoolRange' smart constructor.
data PublicIpv4PoolRange = PublicIpv4PoolRange'
  { -- | The number of addresses in the range.
    addressCount :: Prelude.Maybe Prelude.Int,
    -- | The number of available addresses in the range.
    availableAddressCount :: Prelude.Maybe Prelude.Int,
    -- | The first IP address in the range.
    firstAddress :: Prelude.Maybe Prelude.Text,
    -- | The last IP address in the range.
    lastAddress :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'availableAddressCount', 'publicIpv4PoolRange_availableAddressCount' - The number of available addresses in the range.
--
-- 'firstAddress', 'publicIpv4PoolRange_firstAddress' - The first IP address in the range.
--
-- 'lastAddress', 'publicIpv4PoolRange_lastAddress' - The last IP address in the range.
newPublicIpv4PoolRange ::
  PublicIpv4PoolRange
newPublicIpv4PoolRange =
  PublicIpv4PoolRange'
    { addressCount =
        Prelude.Nothing,
      availableAddressCount = Prelude.Nothing,
      firstAddress = Prelude.Nothing,
      lastAddress = Prelude.Nothing
    }

-- | The number of addresses in the range.
publicIpv4PoolRange_addressCount :: Lens.Lens' PublicIpv4PoolRange (Prelude.Maybe Prelude.Int)
publicIpv4PoolRange_addressCount = Lens.lens (\PublicIpv4PoolRange' {addressCount} -> addressCount) (\s@PublicIpv4PoolRange' {} a -> s {addressCount = a} :: PublicIpv4PoolRange)

-- | The number of available addresses in the range.
publicIpv4PoolRange_availableAddressCount :: Lens.Lens' PublicIpv4PoolRange (Prelude.Maybe Prelude.Int)
publicIpv4PoolRange_availableAddressCount = Lens.lens (\PublicIpv4PoolRange' {availableAddressCount} -> availableAddressCount) (\s@PublicIpv4PoolRange' {} a -> s {availableAddressCount = a} :: PublicIpv4PoolRange)

-- | The first IP address in the range.
publicIpv4PoolRange_firstAddress :: Lens.Lens' PublicIpv4PoolRange (Prelude.Maybe Prelude.Text)
publicIpv4PoolRange_firstAddress = Lens.lens (\PublicIpv4PoolRange' {firstAddress} -> firstAddress) (\s@PublicIpv4PoolRange' {} a -> s {firstAddress = a} :: PublicIpv4PoolRange)

-- | The last IP address in the range.
publicIpv4PoolRange_lastAddress :: Lens.Lens' PublicIpv4PoolRange (Prelude.Maybe Prelude.Text)
publicIpv4PoolRange_lastAddress = Lens.lens (\PublicIpv4PoolRange' {lastAddress} -> lastAddress) (\s@PublicIpv4PoolRange' {} a -> s {lastAddress = a} :: PublicIpv4PoolRange)

instance Data.FromXML PublicIpv4PoolRange where
  parseXML x =
    PublicIpv4PoolRange'
      Prelude.<$> (x Data..@? "addressCount")
      Prelude.<*> (x Data..@? "availableAddressCount")
      Prelude.<*> (x Data..@? "firstAddress")
      Prelude.<*> (x Data..@? "lastAddress")

instance Prelude.Hashable PublicIpv4PoolRange where
  hashWithSalt _salt PublicIpv4PoolRange' {..} =
    _salt
      `Prelude.hashWithSalt` addressCount
      `Prelude.hashWithSalt` availableAddressCount
      `Prelude.hashWithSalt` firstAddress
      `Prelude.hashWithSalt` lastAddress

instance Prelude.NFData PublicIpv4PoolRange where
  rnf PublicIpv4PoolRange' {..} =
    Prelude.rnf addressCount
      `Prelude.seq` Prelude.rnf availableAddressCount
      `Prelude.seq` Prelude.rnf firstAddress
      `Prelude.seq` Prelude.rnf lastAddress

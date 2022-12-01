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
-- Module      : Amazonka.SecurityHub.Types.AwsEc2NetworkInterfaceIpV6AddressDetail
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEc2NetworkInterfaceIpV6AddressDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides information about an IPV6 address that is associated with the
-- network interface.
--
-- /See:/ 'newAwsEc2NetworkInterfaceIpV6AddressDetail' smart constructor.
data AwsEc2NetworkInterfaceIpV6AddressDetail = AwsEc2NetworkInterfaceIpV6AddressDetail'
  { -- | The IPV6 address.
    ipV6Address :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEc2NetworkInterfaceIpV6AddressDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipV6Address', 'awsEc2NetworkInterfaceIpV6AddressDetail_ipV6Address' - The IPV6 address.
newAwsEc2NetworkInterfaceIpV6AddressDetail ::
  AwsEc2NetworkInterfaceIpV6AddressDetail
newAwsEc2NetworkInterfaceIpV6AddressDetail =
  AwsEc2NetworkInterfaceIpV6AddressDetail'
    { ipV6Address =
        Prelude.Nothing
    }

-- | The IPV6 address.
awsEc2NetworkInterfaceIpV6AddressDetail_ipV6Address :: Lens.Lens' AwsEc2NetworkInterfaceIpV6AddressDetail (Prelude.Maybe Prelude.Text)
awsEc2NetworkInterfaceIpV6AddressDetail_ipV6Address = Lens.lens (\AwsEc2NetworkInterfaceIpV6AddressDetail' {ipV6Address} -> ipV6Address) (\s@AwsEc2NetworkInterfaceIpV6AddressDetail' {} a -> s {ipV6Address = a} :: AwsEc2NetworkInterfaceIpV6AddressDetail)

instance
  Core.FromJSON
    AwsEc2NetworkInterfaceIpV6AddressDetail
  where
  parseJSON =
    Core.withObject
      "AwsEc2NetworkInterfaceIpV6AddressDetail"
      ( \x ->
          AwsEc2NetworkInterfaceIpV6AddressDetail'
            Prelude.<$> (x Core..:? "IpV6Address")
      )

instance
  Prelude.Hashable
    AwsEc2NetworkInterfaceIpV6AddressDetail
  where
  hashWithSalt
    _salt
    AwsEc2NetworkInterfaceIpV6AddressDetail' {..} =
      _salt `Prelude.hashWithSalt` ipV6Address

instance
  Prelude.NFData
    AwsEc2NetworkInterfaceIpV6AddressDetail
  where
  rnf AwsEc2NetworkInterfaceIpV6AddressDetail' {..} =
    Prelude.rnf ipV6Address

instance
  Core.ToJSON
    AwsEc2NetworkInterfaceIpV6AddressDetail
  where
  toJSON AwsEc2NetworkInterfaceIpV6AddressDetail' {..} =
    Core.object
      ( Prelude.catMaybes
          [("IpV6Address" Core..=) Prelude.<$> ipV6Address]
      )

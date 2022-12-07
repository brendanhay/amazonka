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
-- Module      : Amazonka.SecurityHub.Types.AwsEc2SecurityGroupIpv6Range
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEc2SecurityGroupIpv6Range where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A range of IPv6 addresses.
--
-- /See:/ 'newAwsEc2SecurityGroupIpv6Range' smart constructor.
data AwsEc2SecurityGroupIpv6Range = AwsEc2SecurityGroupIpv6Range'
  { -- | The IPv6 CIDR range. You can specify either a CIDR range or a source
    -- security group, but not both. To specify a single IPv6 address, use the
    -- \/128 prefix length.
    cidrIpv6 :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEc2SecurityGroupIpv6Range' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cidrIpv6', 'awsEc2SecurityGroupIpv6Range_cidrIpv6' - The IPv6 CIDR range. You can specify either a CIDR range or a source
-- security group, but not both. To specify a single IPv6 address, use the
-- \/128 prefix length.
newAwsEc2SecurityGroupIpv6Range ::
  AwsEc2SecurityGroupIpv6Range
newAwsEc2SecurityGroupIpv6Range =
  AwsEc2SecurityGroupIpv6Range'
    { cidrIpv6 =
        Prelude.Nothing
    }

-- | The IPv6 CIDR range. You can specify either a CIDR range or a source
-- security group, but not both. To specify a single IPv6 address, use the
-- \/128 prefix length.
awsEc2SecurityGroupIpv6Range_cidrIpv6 :: Lens.Lens' AwsEc2SecurityGroupIpv6Range (Prelude.Maybe Prelude.Text)
awsEc2SecurityGroupIpv6Range_cidrIpv6 = Lens.lens (\AwsEc2SecurityGroupIpv6Range' {cidrIpv6} -> cidrIpv6) (\s@AwsEc2SecurityGroupIpv6Range' {} a -> s {cidrIpv6 = a} :: AwsEc2SecurityGroupIpv6Range)

instance Data.FromJSON AwsEc2SecurityGroupIpv6Range where
  parseJSON =
    Data.withObject
      "AwsEc2SecurityGroupIpv6Range"
      ( \x ->
          AwsEc2SecurityGroupIpv6Range'
            Prelude.<$> (x Data..:? "CidrIpv6")
      )

instance
  Prelude.Hashable
    AwsEc2SecurityGroupIpv6Range
  where
  hashWithSalt _salt AwsEc2SecurityGroupIpv6Range' {..} =
    _salt `Prelude.hashWithSalt` cidrIpv6

instance Prelude.NFData AwsEc2SecurityGroupIpv6Range where
  rnf AwsEc2SecurityGroupIpv6Range' {..} =
    Prelude.rnf cidrIpv6

instance Data.ToJSON AwsEc2SecurityGroupIpv6Range where
  toJSON AwsEc2SecurityGroupIpv6Range' {..} =
    Data.object
      ( Prelude.catMaybes
          [("CidrIpv6" Data..=) Prelude.<$> cidrIpv6]
      )

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
-- Module      : Amazonka.SecurityHub.Types.AwsEc2SecurityGroupIpRange
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEc2SecurityGroupIpRange where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A range of IPv4 addresses.
--
-- /See:/ 'newAwsEc2SecurityGroupIpRange' smart constructor.
data AwsEc2SecurityGroupIpRange = AwsEc2SecurityGroupIpRange'
  { -- | The IPv4 CIDR range. You can specify either a CIDR range or a source
    -- security group, but not both. To specify a single IPv4 address, use the
    -- \/32 prefix length.
    cidrIp :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEc2SecurityGroupIpRange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cidrIp', 'awsEc2SecurityGroupIpRange_cidrIp' - The IPv4 CIDR range. You can specify either a CIDR range or a source
-- security group, but not both. To specify a single IPv4 address, use the
-- \/32 prefix length.
newAwsEc2SecurityGroupIpRange ::
  AwsEc2SecurityGroupIpRange
newAwsEc2SecurityGroupIpRange =
  AwsEc2SecurityGroupIpRange'
    { cidrIp =
        Prelude.Nothing
    }

-- | The IPv4 CIDR range. You can specify either a CIDR range or a source
-- security group, but not both. To specify a single IPv4 address, use the
-- \/32 prefix length.
awsEc2SecurityGroupIpRange_cidrIp :: Lens.Lens' AwsEc2SecurityGroupIpRange (Prelude.Maybe Prelude.Text)
awsEc2SecurityGroupIpRange_cidrIp = Lens.lens (\AwsEc2SecurityGroupIpRange' {cidrIp} -> cidrIp) (\s@AwsEc2SecurityGroupIpRange' {} a -> s {cidrIp = a} :: AwsEc2SecurityGroupIpRange)

instance Data.FromJSON AwsEc2SecurityGroupIpRange where
  parseJSON =
    Data.withObject
      "AwsEc2SecurityGroupIpRange"
      ( \x ->
          AwsEc2SecurityGroupIpRange'
            Prelude.<$> (x Data..:? "CidrIp")
      )

instance Prelude.Hashable AwsEc2SecurityGroupIpRange where
  hashWithSalt _salt AwsEc2SecurityGroupIpRange' {..} =
    _salt `Prelude.hashWithSalt` cidrIp

instance Prelude.NFData AwsEc2SecurityGroupIpRange where
  rnf AwsEc2SecurityGroupIpRange' {..} =
    Prelude.rnf cidrIp

instance Data.ToJSON AwsEc2SecurityGroupIpRange where
  toJSON AwsEc2SecurityGroupIpRange' {..} =
    Data.object
      ( Prelude.catMaybes
          [("CidrIp" Data..=) Prelude.<$> cidrIp]
      )

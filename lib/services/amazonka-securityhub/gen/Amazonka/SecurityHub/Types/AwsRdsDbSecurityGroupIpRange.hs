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
-- Module      : Amazonka.SecurityHub.Types.AwsRdsDbSecurityGroupIpRange
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsRdsDbSecurityGroupIpRange where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | IP range information for an RDS DB security group.
--
-- /See:/ 'newAwsRdsDbSecurityGroupIpRange' smart constructor.
data AwsRdsDbSecurityGroupIpRange = AwsRdsDbSecurityGroupIpRange'
  { -- | Specifies the IP range.
    cidrIp :: Prelude.Maybe Prelude.Text,
    -- | Specifies the status of the IP range.
    status :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsRdsDbSecurityGroupIpRange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cidrIp', 'awsRdsDbSecurityGroupIpRange_cidrIp' - Specifies the IP range.
--
-- 'status', 'awsRdsDbSecurityGroupIpRange_status' - Specifies the status of the IP range.
newAwsRdsDbSecurityGroupIpRange ::
  AwsRdsDbSecurityGroupIpRange
newAwsRdsDbSecurityGroupIpRange =
  AwsRdsDbSecurityGroupIpRange'
    { cidrIp =
        Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | Specifies the IP range.
awsRdsDbSecurityGroupIpRange_cidrIp :: Lens.Lens' AwsRdsDbSecurityGroupIpRange (Prelude.Maybe Prelude.Text)
awsRdsDbSecurityGroupIpRange_cidrIp = Lens.lens (\AwsRdsDbSecurityGroupIpRange' {cidrIp} -> cidrIp) (\s@AwsRdsDbSecurityGroupIpRange' {} a -> s {cidrIp = a} :: AwsRdsDbSecurityGroupIpRange)

-- | Specifies the status of the IP range.
awsRdsDbSecurityGroupIpRange_status :: Lens.Lens' AwsRdsDbSecurityGroupIpRange (Prelude.Maybe Prelude.Text)
awsRdsDbSecurityGroupIpRange_status = Lens.lens (\AwsRdsDbSecurityGroupIpRange' {status} -> status) (\s@AwsRdsDbSecurityGroupIpRange' {} a -> s {status = a} :: AwsRdsDbSecurityGroupIpRange)

instance Data.FromJSON AwsRdsDbSecurityGroupIpRange where
  parseJSON =
    Data.withObject
      "AwsRdsDbSecurityGroupIpRange"
      ( \x ->
          AwsRdsDbSecurityGroupIpRange'
            Prelude.<$> (x Data..:? "CidrIp")
            Prelude.<*> (x Data..:? "Status")
      )

instance
  Prelude.Hashable
    AwsRdsDbSecurityGroupIpRange
  where
  hashWithSalt _salt AwsRdsDbSecurityGroupIpRange' {..} =
    _salt
      `Prelude.hashWithSalt` cidrIp
      `Prelude.hashWithSalt` status

instance Prelude.NFData AwsRdsDbSecurityGroupIpRange where
  rnf AwsRdsDbSecurityGroupIpRange' {..} =
    Prelude.rnf cidrIp `Prelude.seq` Prelude.rnf status

instance Data.ToJSON AwsRdsDbSecurityGroupIpRange where
  toJSON AwsRdsDbSecurityGroupIpRange' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CidrIp" Data..=) Prelude.<$> cidrIp,
            ("Status" Data..=) Prelude.<$> status
          ]
      )

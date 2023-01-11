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
-- Module      : Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataInstanceRequirementsBaselineEbsBandwidthMbpsDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataInstanceRequirementsBaselineEbsBandwidthMbpsDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The minimum and maximum baseline bandwidth to Amazon Elastic Block Store
-- (Amazon EBS), in Mbps. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-optimized.html Amazon EBS–optimized instances>
-- in the /Amazon EC2 User Guide/.
--
-- /See:/ 'newAwsEc2LaunchTemplateDataInstanceRequirementsBaselineEbsBandwidthMbpsDetails' smart constructor.
data AwsEc2LaunchTemplateDataInstanceRequirementsBaselineEbsBandwidthMbpsDetails = AwsEc2LaunchTemplateDataInstanceRequirementsBaselineEbsBandwidthMbpsDetails'
  { -- | The maximum baseline bandwidth, in Mbps. If this parameter is omitted,
    -- there\'s no maximum limit.
    max :: Prelude.Maybe Prelude.Int,
    -- | The minimum baseline bandwidth, in Mbps. If this parameter is omitted,
    -- there\'s no minimum limit.
    min :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEc2LaunchTemplateDataInstanceRequirementsBaselineEbsBandwidthMbpsDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'max', 'awsEc2LaunchTemplateDataInstanceRequirementsBaselineEbsBandwidthMbpsDetails_max' - The maximum baseline bandwidth, in Mbps. If this parameter is omitted,
-- there\'s no maximum limit.
--
-- 'min', 'awsEc2LaunchTemplateDataInstanceRequirementsBaselineEbsBandwidthMbpsDetails_min' - The minimum baseline bandwidth, in Mbps. If this parameter is omitted,
-- there\'s no minimum limit.
newAwsEc2LaunchTemplateDataInstanceRequirementsBaselineEbsBandwidthMbpsDetails ::
  AwsEc2LaunchTemplateDataInstanceRequirementsBaselineEbsBandwidthMbpsDetails
newAwsEc2LaunchTemplateDataInstanceRequirementsBaselineEbsBandwidthMbpsDetails =
  AwsEc2LaunchTemplateDataInstanceRequirementsBaselineEbsBandwidthMbpsDetails'
    { max =
        Prelude.Nothing,
      min =
        Prelude.Nothing
    }

-- | The maximum baseline bandwidth, in Mbps. If this parameter is omitted,
-- there\'s no maximum limit.
awsEc2LaunchTemplateDataInstanceRequirementsBaselineEbsBandwidthMbpsDetails_max :: Lens.Lens' AwsEc2LaunchTemplateDataInstanceRequirementsBaselineEbsBandwidthMbpsDetails (Prelude.Maybe Prelude.Int)
awsEc2LaunchTemplateDataInstanceRequirementsBaselineEbsBandwidthMbpsDetails_max = Lens.lens (\AwsEc2LaunchTemplateDataInstanceRequirementsBaselineEbsBandwidthMbpsDetails' {max} -> max) (\s@AwsEc2LaunchTemplateDataInstanceRequirementsBaselineEbsBandwidthMbpsDetails' {} a -> s {max = a} :: AwsEc2LaunchTemplateDataInstanceRequirementsBaselineEbsBandwidthMbpsDetails)

-- | The minimum baseline bandwidth, in Mbps. If this parameter is omitted,
-- there\'s no minimum limit.
awsEc2LaunchTemplateDataInstanceRequirementsBaselineEbsBandwidthMbpsDetails_min :: Lens.Lens' AwsEc2LaunchTemplateDataInstanceRequirementsBaselineEbsBandwidthMbpsDetails (Prelude.Maybe Prelude.Int)
awsEc2LaunchTemplateDataInstanceRequirementsBaselineEbsBandwidthMbpsDetails_min = Lens.lens (\AwsEc2LaunchTemplateDataInstanceRequirementsBaselineEbsBandwidthMbpsDetails' {min} -> min) (\s@AwsEc2LaunchTemplateDataInstanceRequirementsBaselineEbsBandwidthMbpsDetails' {} a -> s {min = a} :: AwsEc2LaunchTemplateDataInstanceRequirementsBaselineEbsBandwidthMbpsDetails)

instance
  Data.FromJSON
    AwsEc2LaunchTemplateDataInstanceRequirementsBaselineEbsBandwidthMbpsDetails
  where
  parseJSON =
    Data.withObject
      "AwsEc2LaunchTemplateDataInstanceRequirementsBaselineEbsBandwidthMbpsDetails"
      ( \x ->
          AwsEc2LaunchTemplateDataInstanceRequirementsBaselineEbsBandwidthMbpsDetails'
            Prelude.<$> (x Data..:? "Max") Prelude.<*> (x Data..:? "Min")
      )

instance
  Prelude.Hashable
    AwsEc2LaunchTemplateDataInstanceRequirementsBaselineEbsBandwidthMbpsDetails
  where
  hashWithSalt
    _salt
    AwsEc2LaunchTemplateDataInstanceRequirementsBaselineEbsBandwidthMbpsDetails' {..} =
      _salt `Prelude.hashWithSalt` max
        `Prelude.hashWithSalt` min

instance
  Prelude.NFData
    AwsEc2LaunchTemplateDataInstanceRequirementsBaselineEbsBandwidthMbpsDetails
  where
  rnf
    AwsEc2LaunchTemplateDataInstanceRequirementsBaselineEbsBandwidthMbpsDetails' {..} =
      Prelude.rnf max `Prelude.seq` Prelude.rnf min

instance
  Data.ToJSON
    AwsEc2LaunchTemplateDataInstanceRequirementsBaselineEbsBandwidthMbpsDetails
  where
  toJSON
    AwsEc2LaunchTemplateDataInstanceRequirementsBaselineEbsBandwidthMbpsDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("Max" Data..=) Prelude.<$> max,
              ("Min" Data..=) Prelude.<$> min
            ]
        )

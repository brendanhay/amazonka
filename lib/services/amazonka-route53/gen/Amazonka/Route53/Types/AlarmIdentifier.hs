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
-- Module      : Amazonka.Route53.Types.AlarmIdentifier
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53.Types.AlarmIdentifier where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53.Internal
import Amazonka.Route53.Types.CloudWatchRegion

-- | A complex type that identifies the CloudWatch alarm that you want Amazon
-- Route 53 health checkers to use to determine whether the specified
-- health check is healthy.
--
-- /See:/ 'newAlarmIdentifier' smart constructor.
data AlarmIdentifier = AlarmIdentifier'
  { -- | For the CloudWatch alarm that you want Route 53 health checkers to use
    -- to determine whether this health check is healthy, the region that the
    -- alarm was created in.
    --
    -- For the current list of CloudWatch regions, see
    -- <https://docs.aws.amazon.com/general/latest/gr/cw_region.html Amazon CloudWatch endpoints and quotas>
    -- in the /Amazon Web Services General Reference/.
    region :: CloudWatchRegion,
    -- | The name of the CloudWatch alarm that you want Amazon Route 53 health
    -- checkers to use to determine whether this health check is healthy.
    --
    -- Route 53 supports CloudWatch alarms with the following features:
    --
    -- -   Standard-resolution metrics. High-resolution metrics aren\'t
    --     supported. For more information, see
    --     <https://docs.aws.amazon.com/AmazonCloudWatch/latest/DeveloperGuide/publishingMetrics.html#high-resolution-metrics High-Resolution Metrics>
    --     in the /Amazon CloudWatch User Guide/.
    --
    -- -   Statistics: Average, Minimum, Maximum, Sum, and SampleCount.
    --     Extended statistics aren\'t supported.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AlarmIdentifier' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'region', 'alarmIdentifier_region' - For the CloudWatch alarm that you want Route 53 health checkers to use
-- to determine whether this health check is healthy, the region that the
-- alarm was created in.
--
-- For the current list of CloudWatch regions, see
-- <https://docs.aws.amazon.com/general/latest/gr/cw_region.html Amazon CloudWatch endpoints and quotas>
-- in the /Amazon Web Services General Reference/.
--
-- 'name', 'alarmIdentifier_name' - The name of the CloudWatch alarm that you want Amazon Route 53 health
-- checkers to use to determine whether this health check is healthy.
--
-- Route 53 supports CloudWatch alarms with the following features:
--
-- -   Standard-resolution metrics. High-resolution metrics aren\'t
--     supported. For more information, see
--     <https://docs.aws.amazon.com/AmazonCloudWatch/latest/DeveloperGuide/publishingMetrics.html#high-resolution-metrics High-Resolution Metrics>
--     in the /Amazon CloudWatch User Guide/.
--
-- -   Statistics: Average, Minimum, Maximum, Sum, and SampleCount.
--     Extended statistics aren\'t supported.
newAlarmIdentifier ::
  -- | 'region'
  CloudWatchRegion ->
  -- | 'name'
  Prelude.Text ->
  AlarmIdentifier
newAlarmIdentifier pRegion_ pName_ =
  AlarmIdentifier' {region = pRegion_, name = pName_}

-- | For the CloudWatch alarm that you want Route 53 health checkers to use
-- to determine whether this health check is healthy, the region that the
-- alarm was created in.
--
-- For the current list of CloudWatch regions, see
-- <https://docs.aws.amazon.com/general/latest/gr/cw_region.html Amazon CloudWatch endpoints and quotas>
-- in the /Amazon Web Services General Reference/.
alarmIdentifier_region :: Lens.Lens' AlarmIdentifier CloudWatchRegion
alarmIdentifier_region = Lens.lens (\AlarmIdentifier' {region} -> region) (\s@AlarmIdentifier' {} a -> s {region = a} :: AlarmIdentifier)

-- | The name of the CloudWatch alarm that you want Amazon Route 53 health
-- checkers to use to determine whether this health check is healthy.
--
-- Route 53 supports CloudWatch alarms with the following features:
--
-- -   Standard-resolution metrics. High-resolution metrics aren\'t
--     supported. For more information, see
--     <https://docs.aws.amazon.com/AmazonCloudWatch/latest/DeveloperGuide/publishingMetrics.html#high-resolution-metrics High-Resolution Metrics>
--     in the /Amazon CloudWatch User Guide/.
--
-- -   Statistics: Average, Minimum, Maximum, Sum, and SampleCount.
--     Extended statistics aren\'t supported.
alarmIdentifier_name :: Lens.Lens' AlarmIdentifier Prelude.Text
alarmIdentifier_name = Lens.lens (\AlarmIdentifier' {name} -> name) (\s@AlarmIdentifier' {} a -> s {name = a} :: AlarmIdentifier)

instance Data.FromXML AlarmIdentifier where
  parseXML x =
    AlarmIdentifier'
      Prelude.<$> (x Data..@ "Region")
      Prelude.<*> (x Data..@ "Name")

instance Prelude.Hashable AlarmIdentifier where
  hashWithSalt _salt AlarmIdentifier' {..} =
    _salt
      `Prelude.hashWithSalt` region
      `Prelude.hashWithSalt` name

instance Prelude.NFData AlarmIdentifier where
  rnf AlarmIdentifier' {..} =
    Prelude.rnf region `Prelude.seq` Prelude.rnf name

instance Data.ToXML AlarmIdentifier where
  toXML AlarmIdentifier' {..} =
    Prelude.mconcat
      ["Region" Data.@= region, "Name" Data.@= name]

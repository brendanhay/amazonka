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
-- Module      : Network.AWS.Kinesis.Types.EnhancedMetrics
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kinesis.Types.EnhancedMetrics where

import qualified Network.AWS.Core as Core
import Network.AWS.Kinesis.Types.MetricsName
import qualified Network.AWS.Lens as Lens

-- | Represents enhanced metrics types.
--
-- /See:/ 'newEnhancedMetrics' smart constructor.
data EnhancedMetrics = EnhancedMetrics'
  { -- | List of shard-level metrics.
    --
    -- The following are the valid shard-level metrics. The value \"@ALL@\"
    -- enhances every metric.
    --
    -- -   @IncomingBytes@
    --
    -- -   @IncomingRecords@
    --
    -- -   @OutgoingBytes@
    --
    -- -   @OutgoingRecords@
    --
    -- -   @WriteProvisionedThroughputExceeded@
    --
    -- -   @ReadProvisionedThroughputExceeded@
    --
    -- -   @IteratorAgeMilliseconds@
    --
    -- -   @ALL@
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/kinesis/latest/dev/monitoring-with-cloudwatch.html Monitoring the Amazon Kinesis Data Streams Service with Amazon CloudWatch>
    -- in the /Amazon Kinesis Data Streams Developer Guide/.
    shardLevelMetrics :: Core.Maybe [MetricsName]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EnhancedMetrics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'shardLevelMetrics', 'enhancedMetrics_shardLevelMetrics' - List of shard-level metrics.
--
-- The following are the valid shard-level metrics. The value \"@ALL@\"
-- enhances every metric.
--
-- -   @IncomingBytes@
--
-- -   @IncomingRecords@
--
-- -   @OutgoingBytes@
--
-- -   @OutgoingRecords@
--
-- -   @WriteProvisionedThroughputExceeded@
--
-- -   @ReadProvisionedThroughputExceeded@
--
-- -   @IteratorAgeMilliseconds@
--
-- -   @ALL@
--
-- For more information, see
-- <https://docs.aws.amazon.com/kinesis/latest/dev/monitoring-with-cloudwatch.html Monitoring the Amazon Kinesis Data Streams Service with Amazon CloudWatch>
-- in the /Amazon Kinesis Data Streams Developer Guide/.
newEnhancedMetrics ::
  EnhancedMetrics
newEnhancedMetrics =
  EnhancedMetrics' {shardLevelMetrics = Core.Nothing}

-- | List of shard-level metrics.
--
-- The following are the valid shard-level metrics. The value \"@ALL@\"
-- enhances every metric.
--
-- -   @IncomingBytes@
--
-- -   @IncomingRecords@
--
-- -   @OutgoingBytes@
--
-- -   @OutgoingRecords@
--
-- -   @WriteProvisionedThroughputExceeded@
--
-- -   @ReadProvisionedThroughputExceeded@
--
-- -   @IteratorAgeMilliseconds@
--
-- -   @ALL@
--
-- For more information, see
-- <https://docs.aws.amazon.com/kinesis/latest/dev/monitoring-with-cloudwatch.html Monitoring the Amazon Kinesis Data Streams Service with Amazon CloudWatch>
-- in the /Amazon Kinesis Data Streams Developer Guide/.
enhancedMetrics_shardLevelMetrics :: Lens.Lens' EnhancedMetrics (Core.Maybe [MetricsName])
enhancedMetrics_shardLevelMetrics = Lens.lens (\EnhancedMetrics' {shardLevelMetrics} -> shardLevelMetrics) (\s@EnhancedMetrics' {} a -> s {shardLevelMetrics = a} :: EnhancedMetrics) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON EnhancedMetrics where
  parseJSON =
    Core.withObject
      "EnhancedMetrics"
      ( \x ->
          EnhancedMetrics'
            Core.<$> ( x Core..:? "ShardLevelMetrics"
                         Core..!= Core.mempty
                     )
      )

instance Core.Hashable EnhancedMetrics

instance Core.NFData EnhancedMetrics

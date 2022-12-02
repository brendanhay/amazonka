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
-- Module      : Amazonka.Kinesis.Types.EnhancedMetrics
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kinesis.Types.EnhancedMetrics where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kinesis.Types.MetricsName
import qualified Amazonka.Prelude as Prelude

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
    shardLevelMetrics :: Prelude.Maybe [MetricsName]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  EnhancedMetrics'
    { shardLevelMetrics =
        Prelude.Nothing
    }

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
enhancedMetrics_shardLevelMetrics :: Lens.Lens' EnhancedMetrics (Prelude.Maybe [MetricsName])
enhancedMetrics_shardLevelMetrics = Lens.lens (\EnhancedMetrics' {shardLevelMetrics} -> shardLevelMetrics) (\s@EnhancedMetrics' {} a -> s {shardLevelMetrics = a} :: EnhancedMetrics) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON EnhancedMetrics where
  parseJSON =
    Data.withObject
      "EnhancedMetrics"
      ( \x ->
          EnhancedMetrics'
            Prelude.<$> ( x Data..:? "ShardLevelMetrics"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable EnhancedMetrics where
  hashWithSalt _salt EnhancedMetrics' {..} =
    _salt `Prelude.hashWithSalt` shardLevelMetrics

instance Prelude.NFData EnhancedMetrics where
  rnf EnhancedMetrics' {..} =
    Prelude.rnf shardLevelMetrics

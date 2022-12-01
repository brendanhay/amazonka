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
-- Module      : Amazonka.QLDB.Types.KinesisConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QLDB.Types.KinesisConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The configuration settings of the Amazon Kinesis Data Streams
-- destination for an Amazon QLDB journal stream.
--
-- /See:/ 'newKinesisConfiguration' smart constructor.
data KinesisConfiguration = KinesisConfiguration'
  { -- | Enables QLDB to publish multiple data records in a single Kinesis Data
    -- Streams record, increasing the number of records sent per API call.
    --
    -- /This option is enabled by default./ Record aggregation has important
    -- implications for processing records and requires de-aggregation in your
    -- stream consumer. To learn more, see
    -- <https://docs.aws.amazon.com/streams/latest/dev/kinesis-kpl-concepts.html KPL Key Concepts>
    -- and
    -- <https://docs.aws.amazon.com/streams/latest/dev/kinesis-kpl-consumer-deaggregation.html Consumer De-aggregation>
    -- in the /Amazon Kinesis Data Streams Developer Guide/.
    aggregationEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Name (ARN) of the Kinesis Data Streams resource.
    streamArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KinesisConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'aggregationEnabled', 'kinesisConfiguration_aggregationEnabled' - Enables QLDB to publish multiple data records in a single Kinesis Data
-- Streams record, increasing the number of records sent per API call.
--
-- /This option is enabled by default./ Record aggregation has important
-- implications for processing records and requires de-aggregation in your
-- stream consumer. To learn more, see
-- <https://docs.aws.amazon.com/streams/latest/dev/kinesis-kpl-concepts.html KPL Key Concepts>
-- and
-- <https://docs.aws.amazon.com/streams/latest/dev/kinesis-kpl-consumer-deaggregation.html Consumer De-aggregation>
-- in the /Amazon Kinesis Data Streams Developer Guide/.
--
-- 'streamArn', 'kinesisConfiguration_streamArn' - The Amazon Resource Name (ARN) of the Kinesis Data Streams resource.
newKinesisConfiguration ::
  -- | 'streamArn'
  Prelude.Text ->
  KinesisConfiguration
newKinesisConfiguration pStreamArn_ =
  KinesisConfiguration'
    { aggregationEnabled =
        Prelude.Nothing,
      streamArn = pStreamArn_
    }

-- | Enables QLDB to publish multiple data records in a single Kinesis Data
-- Streams record, increasing the number of records sent per API call.
--
-- /This option is enabled by default./ Record aggregation has important
-- implications for processing records and requires de-aggregation in your
-- stream consumer. To learn more, see
-- <https://docs.aws.amazon.com/streams/latest/dev/kinesis-kpl-concepts.html KPL Key Concepts>
-- and
-- <https://docs.aws.amazon.com/streams/latest/dev/kinesis-kpl-consumer-deaggregation.html Consumer De-aggregation>
-- in the /Amazon Kinesis Data Streams Developer Guide/.
kinesisConfiguration_aggregationEnabled :: Lens.Lens' KinesisConfiguration (Prelude.Maybe Prelude.Bool)
kinesisConfiguration_aggregationEnabled = Lens.lens (\KinesisConfiguration' {aggregationEnabled} -> aggregationEnabled) (\s@KinesisConfiguration' {} a -> s {aggregationEnabled = a} :: KinesisConfiguration)

-- | The Amazon Resource Name (ARN) of the Kinesis Data Streams resource.
kinesisConfiguration_streamArn :: Lens.Lens' KinesisConfiguration Prelude.Text
kinesisConfiguration_streamArn = Lens.lens (\KinesisConfiguration' {streamArn} -> streamArn) (\s@KinesisConfiguration' {} a -> s {streamArn = a} :: KinesisConfiguration)

instance Core.FromJSON KinesisConfiguration where
  parseJSON =
    Core.withObject
      "KinesisConfiguration"
      ( \x ->
          KinesisConfiguration'
            Prelude.<$> (x Core..:? "AggregationEnabled")
            Prelude.<*> (x Core..: "StreamArn")
      )

instance Prelude.Hashable KinesisConfiguration where
  hashWithSalt _salt KinesisConfiguration' {..} =
    _salt `Prelude.hashWithSalt` aggregationEnabled
      `Prelude.hashWithSalt` streamArn

instance Prelude.NFData KinesisConfiguration where
  rnf KinesisConfiguration' {..} =
    Prelude.rnf aggregationEnabled
      `Prelude.seq` Prelude.rnf streamArn

instance Core.ToJSON KinesisConfiguration where
  toJSON KinesisConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("AggregationEnabled" Core..=)
              Prelude.<$> aggregationEnabled,
            Prelude.Just ("StreamArn" Core..= streamArn)
          ]
      )

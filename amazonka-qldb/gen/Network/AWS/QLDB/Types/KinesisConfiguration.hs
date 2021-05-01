{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.QLDB.Types.KinesisConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.QLDB.Types.KinesisConfiguration where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The configuration settings of the Amazon Kinesis Data Streams
-- destination for your Amazon QLDB journal stream.
--
-- /See:/ 'newKinesisConfiguration' smart constructor.
data KinesisConfiguration = KinesisConfiguration'
  { -- | Enables QLDB to publish multiple data records in a single Kinesis Data
    -- Streams record. To learn more, see
    -- <https://docs.aws.amazon.com/streams/latest/dev/kinesis-kpl-concepts.html KPL Key Concepts>
    -- in the /Amazon Kinesis Data Streams Developer Guide/.
    aggregationEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Name (ARN) of the Kinesis data stream resource.
    streamArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'KinesisConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'aggregationEnabled', 'kinesisConfiguration_aggregationEnabled' - Enables QLDB to publish multiple data records in a single Kinesis Data
-- Streams record. To learn more, see
-- <https://docs.aws.amazon.com/streams/latest/dev/kinesis-kpl-concepts.html KPL Key Concepts>
-- in the /Amazon Kinesis Data Streams Developer Guide/.
--
-- 'streamArn', 'kinesisConfiguration_streamArn' - The Amazon Resource Name (ARN) of the Kinesis data stream resource.
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
-- Streams record. To learn more, see
-- <https://docs.aws.amazon.com/streams/latest/dev/kinesis-kpl-concepts.html KPL Key Concepts>
-- in the /Amazon Kinesis Data Streams Developer Guide/.
kinesisConfiguration_aggregationEnabled :: Lens.Lens' KinesisConfiguration (Prelude.Maybe Prelude.Bool)
kinesisConfiguration_aggregationEnabled = Lens.lens (\KinesisConfiguration' {aggregationEnabled} -> aggregationEnabled) (\s@KinesisConfiguration' {} a -> s {aggregationEnabled = a} :: KinesisConfiguration)

-- | The Amazon Resource Name (ARN) of the Kinesis data stream resource.
kinesisConfiguration_streamArn :: Lens.Lens' KinesisConfiguration Prelude.Text
kinesisConfiguration_streamArn = Lens.lens (\KinesisConfiguration' {streamArn} -> streamArn) (\s@KinesisConfiguration' {} a -> s {streamArn = a} :: KinesisConfiguration)

instance Prelude.FromJSON KinesisConfiguration where
  parseJSON =
    Prelude.withObject
      "KinesisConfiguration"
      ( \x ->
          KinesisConfiguration'
            Prelude.<$> (x Prelude..:? "AggregationEnabled")
            Prelude.<*> (x Prelude..: "StreamArn")
      )

instance Prelude.Hashable KinesisConfiguration

instance Prelude.NFData KinesisConfiguration

instance Prelude.ToJSON KinesisConfiguration where
  toJSON KinesisConfiguration' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("AggregationEnabled" Prelude..=)
              Prelude.<$> aggregationEnabled,
            Prelude.Just ("StreamArn" Prelude..= streamArn)
          ]
      )

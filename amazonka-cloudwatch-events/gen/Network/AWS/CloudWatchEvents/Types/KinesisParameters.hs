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
-- Module      : Network.AWS.CloudWatchEvents.Types.KinesisParameters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.KinesisParameters where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | This object enables you to specify a JSON path to extract from the event
-- and use as the partition key for the Amazon Kinesis data stream, so that
-- you can control the shard to which the event goes. If you do not include
-- this parameter, the default is to use the @eventId@ as the partition
-- key.
--
-- /See:/ 'newKinesisParameters' smart constructor.
data KinesisParameters = KinesisParameters'
  { -- | The JSON path to be extracted from the event and used as the partition
    -- key. For more information, see
    -- <https://docs.aws.amazon.com/streams/latest/dev/key-concepts.html#partition-key Amazon Kinesis Streams Key Concepts>
    -- in the /Amazon Kinesis Streams Developer Guide/.
    partitionKeyPath :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'KinesisParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'partitionKeyPath', 'kinesisParameters_partitionKeyPath' - The JSON path to be extracted from the event and used as the partition
-- key. For more information, see
-- <https://docs.aws.amazon.com/streams/latest/dev/key-concepts.html#partition-key Amazon Kinesis Streams Key Concepts>
-- in the /Amazon Kinesis Streams Developer Guide/.
newKinesisParameters ::
  -- | 'partitionKeyPath'
  Prelude.Text ->
  KinesisParameters
newKinesisParameters pPartitionKeyPath_ =
  KinesisParameters'
    { partitionKeyPath =
        pPartitionKeyPath_
    }

-- | The JSON path to be extracted from the event and used as the partition
-- key. For more information, see
-- <https://docs.aws.amazon.com/streams/latest/dev/key-concepts.html#partition-key Amazon Kinesis Streams Key Concepts>
-- in the /Amazon Kinesis Streams Developer Guide/.
kinesisParameters_partitionKeyPath :: Lens.Lens' KinesisParameters Prelude.Text
kinesisParameters_partitionKeyPath = Lens.lens (\KinesisParameters' {partitionKeyPath} -> partitionKeyPath) (\s@KinesisParameters' {} a -> s {partitionKeyPath = a} :: KinesisParameters)

instance Prelude.FromJSON KinesisParameters where
  parseJSON =
    Prelude.withObject
      "KinesisParameters"
      ( \x ->
          KinesisParameters'
            Prelude.<$> (x Prelude..: "PartitionKeyPath")
      )

instance Prelude.Hashable KinesisParameters

instance Prelude.NFData KinesisParameters

instance Prelude.ToJSON KinesisParameters where
  toJSON KinesisParameters' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("PartitionKeyPath" Prelude..= partitionKeyPath)
          ]
      )

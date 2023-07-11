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
-- Module      : Amazonka.CloudWatchEvents.Types.KinesisParameters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatchEvents.Types.KinesisParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Data.FromJSON KinesisParameters where
  parseJSON =
    Data.withObject
      "KinesisParameters"
      ( \x ->
          KinesisParameters'
            Prelude.<$> (x Data..: "PartitionKeyPath")
      )

instance Prelude.Hashable KinesisParameters where
  hashWithSalt _salt KinesisParameters' {..} =
    _salt `Prelude.hashWithSalt` partitionKeyPath

instance Prelude.NFData KinesisParameters where
  rnf KinesisParameters' {..} =
    Prelude.rnf partitionKeyPath

instance Data.ToJSON KinesisParameters where
  toJSON KinesisParameters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("PartitionKeyPath" Data..= partitionKeyPath)
          ]
      )

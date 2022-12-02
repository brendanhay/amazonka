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
-- Module      : Amazonka.Scheduler.Types.KinesisParameters
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Scheduler.Types.KinesisParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The templated target type for the Amazon Kinesis
-- <kinesis/latest/APIReference/API_PutRecord.html PutRecord> API
-- operation.
--
-- /See:/ 'newKinesisParameters' smart constructor.
data KinesisParameters = KinesisParameters'
  { -- | Specifies the shard to which EventBridge Scheduler sends the event. For
    -- more information, see
    -- <https://docs.aws.amazon.com/streams/latest/dev/key-concepts.html Amazon Kinesis Data Streams terminology and concepts>
    -- in the /Amazon Kinesis Streams Developer Guide/.
    partitionKey :: Prelude.Text
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
-- 'partitionKey', 'kinesisParameters_partitionKey' - Specifies the shard to which EventBridge Scheduler sends the event. For
-- more information, see
-- <https://docs.aws.amazon.com/streams/latest/dev/key-concepts.html Amazon Kinesis Data Streams terminology and concepts>
-- in the /Amazon Kinesis Streams Developer Guide/.
newKinesisParameters ::
  -- | 'partitionKey'
  Prelude.Text ->
  KinesisParameters
newKinesisParameters pPartitionKey_ =
  KinesisParameters' {partitionKey = pPartitionKey_}

-- | Specifies the shard to which EventBridge Scheduler sends the event. For
-- more information, see
-- <https://docs.aws.amazon.com/streams/latest/dev/key-concepts.html Amazon Kinesis Data Streams terminology and concepts>
-- in the /Amazon Kinesis Streams Developer Guide/.
kinesisParameters_partitionKey :: Lens.Lens' KinesisParameters Prelude.Text
kinesisParameters_partitionKey = Lens.lens (\KinesisParameters' {partitionKey} -> partitionKey) (\s@KinesisParameters' {} a -> s {partitionKey = a} :: KinesisParameters)

instance Data.FromJSON KinesisParameters where
  parseJSON =
    Data.withObject
      "KinesisParameters"
      ( \x ->
          KinesisParameters'
            Prelude.<$> (x Data..: "PartitionKey")
      )

instance Prelude.Hashable KinesisParameters where
  hashWithSalt _salt KinesisParameters' {..} =
    _salt `Prelude.hashWithSalt` partitionKey

instance Prelude.NFData KinesisParameters where
  rnf KinesisParameters' {..} = Prelude.rnf partitionKey

instance Data.ToJSON KinesisParameters where
  toJSON KinesisParameters' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("PartitionKey" Data..= partitionKey)]
      )

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
-- Module      : Amazonka.Pipes.Types.PipeSourceManagedStreamingKafkaParameters
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pipes.Types.PipeSourceManagedStreamingKafkaParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pipes.Types.MSKAccessCredentials
import Amazonka.Pipes.Types.MSKStartPosition
import qualified Amazonka.Prelude as Prelude

-- | The parameters for using an MSK stream as a source.
--
-- /See:/ 'newPipeSourceManagedStreamingKafkaParameters' smart constructor.
data PipeSourceManagedStreamingKafkaParameters = PipeSourceManagedStreamingKafkaParameters'
  { -- | The maximum number of records to include in each batch.
    batchSize :: Prelude.Maybe Prelude.Natural,
    -- | The name of the destination queue to consume.
    consumerGroupID :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The credentials needed to access the resource.
    credentials :: Prelude.Maybe MSKAccessCredentials,
    -- | The maximum length of a time to wait for events.
    maximumBatchingWindowInSeconds :: Prelude.Maybe Prelude.Natural,
    -- | (Streams only) The position in a stream from which to start reading.
    startingPosition :: Prelude.Maybe MSKStartPosition,
    -- | The name of the topic that the pipe will read from.
    topicName :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PipeSourceManagedStreamingKafkaParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'batchSize', 'pipeSourceManagedStreamingKafkaParameters_batchSize' - The maximum number of records to include in each batch.
--
-- 'consumerGroupID', 'pipeSourceManagedStreamingKafkaParameters_consumerGroupID' - The name of the destination queue to consume.
--
-- 'credentials', 'pipeSourceManagedStreamingKafkaParameters_credentials' - The credentials needed to access the resource.
--
-- 'maximumBatchingWindowInSeconds', 'pipeSourceManagedStreamingKafkaParameters_maximumBatchingWindowInSeconds' - The maximum length of a time to wait for events.
--
-- 'startingPosition', 'pipeSourceManagedStreamingKafkaParameters_startingPosition' - (Streams only) The position in a stream from which to start reading.
--
-- 'topicName', 'pipeSourceManagedStreamingKafkaParameters_topicName' - The name of the topic that the pipe will read from.
newPipeSourceManagedStreamingKafkaParameters ::
  -- | 'topicName'
  Prelude.Text ->
  PipeSourceManagedStreamingKafkaParameters
newPipeSourceManagedStreamingKafkaParameters
  pTopicName_ =
    PipeSourceManagedStreamingKafkaParameters'
      { batchSize =
          Prelude.Nothing,
        consumerGroupID =
          Prelude.Nothing,
        credentials = Prelude.Nothing,
        maximumBatchingWindowInSeconds =
          Prelude.Nothing,
        startingPosition =
          Prelude.Nothing,
        topicName =
          Data._Sensitive
            Lens.# pTopicName_
      }

-- | The maximum number of records to include in each batch.
pipeSourceManagedStreamingKafkaParameters_batchSize :: Lens.Lens' PipeSourceManagedStreamingKafkaParameters (Prelude.Maybe Prelude.Natural)
pipeSourceManagedStreamingKafkaParameters_batchSize = Lens.lens (\PipeSourceManagedStreamingKafkaParameters' {batchSize} -> batchSize) (\s@PipeSourceManagedStreamingKafkaParameters' {} a -> s {batchSize = a} :: PipeSourceManagedStreamingKafkaParameters)

-- | The name of the destination queue to consume.
pipeSourceManagedStreamingKafkaParameters_consumerGroupID :: Lens.Lens' PipeSourceManagedStreamingKafkaParameters (Prelude.Maybe Prelude.Text)
pipeSourceManagedStreamingKafkaParameters_consumerGroupID = Lens.lens (\PipeSourceManagedStreamingKafkaParameters' {consumerGroupID} -> consumerGroupID) (\s@PipeSourceManagedStreamingKafkaParameters' {} a -> s {consumerGroupID = a} :: PipeSourceManagedStreamingKafkaParameters) Prelude.. Lens.mapping Data._Sensitive

-- | The credentials needed to access the resource.
pipeSourceManagedStreamingKafkaParameters_credentials :: Lens.Lens' PipeSourceManagedStreamingKafkaParameters (Prelude.Maybe MSKAccessCredentials)
pipeSourceManagedStreamingKafkaParameters_credentials = Lens.lens (\PipeSourceManagedStreamingKafkaParameters' {credentials} -> credentials) (\s@PipeSourceManagedStreamingKafkaParameters' {} a -> s {credentials = a} :: PipeSourceManagedStreamingKafkaParameters)

-- | The maximum length of a time to wait for events.
pipeSourceManagedStreamingKafkaParameters_maximumBatchingWindowInSeconds :: Lens.Lens' PipeSourceManagedStreamingKafkaParameters (Prelude.Maybe Prelude.Natural)
pipeSourceManagedStreamingKafkaParameters_maximumBatchingWindowInSeconds = Lens.lens (\PipeSourceManagedStreamingKafkaParameters' {maximumBatchingWindowInSeconds} -> maximumBatchingWindowInSeconds) (\s@PipeSourceManagedStreamingKafkaParameters' {} a -> s {maximumBatchingWindowInSeconds = a} :: PipeSourceManagedStreamingKafkaParameters)

-- | (Streams only) The position in a stream from which to start reading.
pipeSourceManagedStreamingKafkaParameters_startingPosition :: Lens.Lens' PipeSourceManagedStreamingKafkaParameters (Prelude.Maybe MSKStartPosition)
pipeSourceManagedStreamingKafkaParameters_startingPosition = Lens.lens (\PipeSourceManagedStreamingKafkaParameters' {startingPosition} -> startingPosition) (\s@PipeSourceManagedStreamingKafkaParameters' {} a -> s {startingPosition = a} :: PipeSourceManagedStreamingKafkaParameters)

-- | The name of the topic that the pipe will read from.
pipeSourceManagedStreamingKafkaParameters_topicName :: Lens.Lens' PipeSourceManagedStreamingKafkaParameters Prelude.Text
pipeSourceManagedStreamingKafkaParameters_topicName = Lens.lens (\PipeSourceManagedStreamingKafkaParameters' {topicName} -> topicName) (\s@PipeSourceManagedStreamingKafkaParameters' {} a -> s {topicName = a} :: PipeSourceManagedStreamingKafkaParameters) Prelude.. Data._Sensitive

instance
  Data.FromJSON
    PipeSourceManagedStreamingKafkaParameters
  where
  parseJSON =
    Data.withObject
      "PipeSourceManagedStreamingKafkaParameters"
      ( \x ->
          PipeSourceManagedStreamingKafkaParameters'
            Prelude.<$> (x Data..:? "BatchSize")
              Prelude.<*> (x Data..:? "ConsumerGroupID")
              Prelude.<*> (x Data..:? "Credentials")
              Prelude.<*> (x Data..:? "MaximumBatchingWindowInSeconds")
              Prelude.<*> (x Data..:? "StartingPosition")
              Prelude.<*> (x Data..: "TopicName")
      )

instance
  Prelude.Hashable
    PipeSourceManagedStreamingKafkaParameters
  where
  hashWithSalt
    _salt
    PipeSourceManagedStreamingKafkaParameters' {..} =
      _salt `Prelude.hashWithSalt` batchSize
        `Prelude.hashWithSalt` consumerGroupID
        `Prelude.hashWithSalt` credentials
        `Prelude.hashWithSalt` maximumBatchingWindowInSeconds
        `Prelude.hashWithSalt` startingPosition
        `Prelude.hashWithSalt` topicName

instance
  Prelude.NFData
    PipeSourceManagedStreamingKafkaParameters
  where
  rnf PipeSourceManagedStreamingKafkaParameters' {..} =
    Prelude.rnf batchSize
      `Prelude.seq` Prelude.rnf consumerGroupID
      `Prelude.seq` Prelude.rnf credentials
      `Prelude.seq` Prelude.rnf maximumBatchingWindowInSeconds
      `Prelude.seq` Prelude.rnf startingPosition
      `Prelude.seq` Prelude.rnf topicName

instance
  Data.ToJSON
    PipeSourceManagedStreamingKafkaParameters
  where
  toJSON PipeSourceManagedStreamingKafkaParameters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BatchSize" Data..=) Prelude.<$> batchSize,
            ("ConsumerGroupID" Data..=)
              Prelude.<$> consumerGroupID,
            ("Credentials" Data..=) Prelude.<$> credentials,
            ("MaximumBatchingWindowInSeconds" Data..=)
              Prelude.<$> maximumBatchingWindowInSeconds,
            ("StartingPosition" Data..=)
              Prelude.<$> startingPosition,
            Prelude.Just ("TopicName" Data..= topicName)
          ]
      )

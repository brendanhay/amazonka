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
-- Module      : Amazonka.Pipes.Types.PipeSourceActiveMQBrokerParameters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pipes.Types.PipeSourceActiveMQBrokerParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pipes.Types.MQBrokerAccessCredentials
import qualified Amazonka.Prelude as Prelude

-- | The parameters for using an Active MQ broker as a source.
--
-- /See:/ 'newPipeSourceActiveMQBrokerParameters' smart constructor.
data PipeSourceActiveMQBrokerParameters = PipeSourceActiveMQBrokerParameters'
  { -- | The maximum number of records to include in each batch.
    batchSize :: Prelude.Maybe Prelude.Natural,
    -- | The maximum length of a time to wait for events.
    maximumBatchingWindowInSeconds :: Prelude.Maybe Prelude.Natural,
    -- | The credentials needed to access the resource.
    credentials :: MQBrokerAccessCredentials,
    -- | The name of the destination queue to consume.
    queueName :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PipeSourceActiveMQBrokerParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'batchSize', 'pipeSourceActiveMQBrokerParameters_batchSize' - The maximum number of records to include in each batch.
--
-- 'maximumBatchingWindowInSeconds', 'pipeSourceActiveMQBrokerParameters_maximumBatchingWindowInSeconds' - The maximum length of a time to wait for events.
--
-- 'credentials', 'pipeSourceActiveMQBrokerParameters_credentials' - The credentials needed to access the resource.
--
-- 'queueName', 'pipeSourceActiveMQBrokerParameters_queueName' - The name of the destination queue to consume.
newPipeSourceActiveMQBrokerParameters ::
  -- | 'credentials'
  MQBrokerAccessCredentials ->
  -- | 'queueName'
  Prelude.Text ->
  PipeSourceActiveMQBrokerParameters
newPipeSourceActiveMQBrokerParameters
  pCredentials_
  pQueueName_ =
    PipeSourceActiveMQBrokerParameters'
      { batchSize =
          Prelude.Nothing,
        maximumBatchingWindowInSeconds =
          Prelude.Nothing,
        credentials = pCredentials_,
        queueName =
          Data._Sensitive Lens.# pQueueName_
      }

-- | The maximum number of records to include in each batch.
pipeSourceActiveMQBrokerParameters_batchSize :: Lens.Lens' PipeSourceActiveMQBrokerParameters (Prelude.Maybe Prelude.Natural)
pipeSourceActiveMQBrokerParameters_batchSize = Lens.lens (\PipeSourceActiveMQBrokerParameters' {batchSize} -> batchSize) (\s@PipeSourceActiveMQBrokerParameters' {} a -> s {batchSize = a} :: PipeSourceActiveMQBrokerParameters)

-- | The maximum length of a time to wait for events.
pipeSourceActiveMQBrokerParameters_maximumBatchingWindowInSeconds :: Lens.Lens' PipeSourceActiveMQBrokerParameters (Prelude.Maybe Prelude.Natural)
pipeSourceActiveMQBrokerParameters_maximumBatchingWindowInSeconds = Lens.lens (\PipeSourceActiveMQBrokerParameters' {maximumBatchingWindowInSeconds} -> maximumBatchingWindowInSeconds) (\s@PipeSourceActiveMQBrokerParameters' {} a -> s {maximumBatchingWindowInSeconds = a} :: PipeSourceActiveMQBrokerParameters)

-- | The credentials needed to access the resource.
pipeSourceActiveMQBrokerParameters_credentials :: Lens.Lens' PipeSourceActiveMQBrokerParameters MQBrokerAccessCredentials
pipeSourceActiveMQBrokerParameters_credentials = Lens.lens (\PipeSourceActiveMQBrokerParameters' {credentials} -> credentials) (\s@PipeSourceActiveMQBrokerParameters' {} a -> s {credentials = a} :: PipeSourceActiveMQBrokerParameters)

-- | The name of the destination queue to consume.
pipeSourceActiveMQBrokerParameters_queueName :: Lens.Lens' PipeSourceActiveMQBrokerParameters Prelude.Text
pipeSourceActiveMQBrokerParameters_queueName = Lens.lens (\PipeSourceActiveMQBrokerParameters' {queueName} -> queueName) (\s@PipeSourceActiveMQBrokerParameters' {} a -> s {queueName = a} :: PipeSourceActiveMQBrokerParameters) Prelude.. Data._Sensitive

instance
  Data.FromJSON
    PipeSourceActiveMQBrokerParameters
  where
  parseJSON =
    Data.withObject
      "PipeSourceActiveMQBrokerParameters"
      ( \x ->
          PipeSourceActiveMQBrokerParameters'
            Prelude.<$> (x Data..:? "BatchSize")
            Prelude.<*> (x Data..:? "MaximumBatchingWindowInSeconds")
            Prelude.<*> (x Data..: "Credentials")
            Prelude.<*> (x Data..: "QueueName")
      )

instance
  Prelude.Hashable
    PipeSourceActiveMQBrokerParameters
  where
  hashWithSalt
    _salt
    PipeSourceActiveMQBrokerParameters' {..} =
      _salt
        `Prelude.hashWithSalt` batchSize
        `Prelude.hashWithSalt` maximumBatchingWindowInSeconds
        `Prelude.hashWithSalt` credentials
        `Prelude.hashWithSalt` queueName

instance
  Prelude.NFData
    PipeSourceActiveMQBrokerParameters
  where
  rnf PipeSourceActiveMQBrokerParameters' {..} =
    Prelude.rnf batchSize
      `Prelude.seq` Prelude.rnf maximumBatchingWindowInSeconds
      `Prelude.seq` Prelude.rnf credentials
      `Prelude.seq` Prelude.rnf queueName

instance
  Data.ToJSON
    PipeSourceActiveMQBrokerParameters
  where
  toJSON PipeSourceActiveMQBrokerParameters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BatchSize" Data..=) Prelude.<$> batchSize,
            ("MaximumBatchingWindowInSeconds" Data..=)
              Prelude.<$> maximumBatchingWindowInSeconds,
            Prelude.Just ("Credentials" Data..= credentials),
            Prelude.Just ("QueueName" Data..= queueName)
          ]
      )

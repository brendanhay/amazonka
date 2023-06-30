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
-- Module      : Amazonka.Pipes.Types.PipeSourceRabbitMQBrokerParameters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pipes.Types.PipeSourceRabbitMQBrokerParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pipes.Types.MQBrokerAccessCredentials
import qualified Amazonka.Prelude as Prelude

-- | The parameters for using a Rabbit MQ broker as a source.
--
-- /See:/ 'newPipeSourceRabbitMQBrokerParameters' smart constructor.
data PipeSourceRabbitMQBrokerParameters = PipeSourceRabbitMQBrokerParameters'
  { -- | The maximum number of records to include in each batch.
    batchSize :: Prelude.Maybe Prelude.Natural,
    -- | The maximum length of a time to wait for events.
    maximumBatchingWindowInSeconds :: Prelude.Maybe Prelude.Natural,
    -- | The name of the virtual host associated with the source broker.
    virtualHost :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The credentials needed to access the resource.
    credentials :: MQBrokerAccessCredentials,
    -- | The name of the destination queue to consume.
    queueName :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PipeSourceRabbitMQBrokerParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'batchSize', 'pipeSourceRabbitMQBrokerParameters_batchSize' - The maximum number of records to include in each batch.
--
-- 'maximumBatchingWindowInSeconds', 'pipeSourceRabbitMQBrokerParameters_maximumBatchingWindowInSeconds' - The maximum length of a time to wait for events.
--
-- 'virtualHost', 'pipeSourceRabbitMQBrokerParameters_virtualHost' - The name of the virtual host associated with the source broker.
--
-- 'credentials', 'pipeSourceRabbitMQBrokerParameters_credentials' - The credentials needed to access the resource.
--
-- 'queueName', 'pipeSourceRabbitMQBrokerParameters_queueName' - The name of the destination queue to consume.
newPipeSourceRabbitMQBrokerParameters ::
  -- | 'credentials'
  MQBrokerAccessCredentials ->
  -- | 'queueName'
  Prelude.Text ->
  PipeSourceRabbitMQBrokerParameters
newPipeSourceRabbitMQBrokerParameters
  pCredentials_
  pQueueName_ =
    PipeSourceRabbitMQBrokerParameters'
      { batchSize =
          Prelude.Nothing,
        maximumBatchingWindowInSeconds =
          Prelude.Nothing,
        virtualHost = Prelude.Nothing,
        credentials = pCredentials_,
        queueName =
          Data._Sensitive Lens.# pQueueName_
      }

-- | The maximum number of records to include in each batch.
pipeSourceRabbitMQBrokerParameters_batchSize :: Lens.Lens' PipeSourceRabbitMQBrokerParameters (Prelude.Maybe Prelude.Natural)
pipeSourceRabbitMQBrokerParameters_batchSize = Lens.lens (\PipeSourceRabbitMQBrokerParameters' {batchSize} -> batchSize) (\s@PipeSourceRabbitMQBrokerParameters' {} a -> s {batchSize = a} :: PipeSourceRabbitMQBrokerParameters)

-- | The maximum length of a time to wait for events.
pipeSourceRabbitMQBrokerParameters_maximumBatchingWindowInSeconds :: Lens.Lens' PipeSourceRabbitMQBrokerParameters (Prelude.Maybe Prelude.Natural)
pipeSourceRabbitMQBrokerParameters_maximumBatchingWindowInSeconds = Lens.lens (\PipeSourceRabbitMQBrokerParameters' {maximumBatchingWindowInSeconds} -> maximumBatchingWindowInSeconds) (\s@PipeSourceRabbitMQBrokerParameters' {} a -> s {maximumBatchingWindowInSeconds = a} :: PipeSourceRabbitMQBrokerParameters)

-- | The name of the virtual host associated with the source broker.
pipeSourceRabbitMQBrokerParameters_virtualHost :: Lens.Lens' PipeSourceRabbitMQBrokerParameters (Prelude.Maybe Prelude.Text)
pipeSourceRabbitMQBrokerParameters_virtualHost = Lens.lens (\PipeSourceRabbitMQBrokerParameters' {virtualHost} -> virtualHost) (\s@PipeSourceRabbitMQBrokerParameters' {} a -> s {virtualHost = a} :: PipeSourceRabbitMQBrokerParameters) Prelude.. Lens.mapping Data._Sensitive

-- | The credentials needed to access the resource.
pipeSourceRabbitMQBrokerParameters_credentials :: Lens.Lens' PipeSourceRabbitMQBrokerParameters MQBrokerAccessCredentials
pipeSourceRabbitMQBrokerParameters_credentials = Lens.lens (\PipeSourceRabbitMQBrokerParameters' {credentials} -> credentials) (\s@PipeSourceRabbitMQBrokerParameters' {} a -> s {credentials = a} :: PipeSourceRabbitMQBrokerParameters)

-- | The name of the destination queue to consume.
pipeSourceRabbitMQBrokerParameters_queueName :: Lens.Lens' PipeSourceRabbitMQBrokerParameters Prelude.Text
pipeSourceRabbitMQBrokerParameters_queueName = Lens.lens (\PipeSourceRabbitMQBrokerParameters' {queueName} -> queueName) (\s@PipeSourceRabbitMQBrokerParameters' {} a -> s {queueName = a} :: PipeSourceRabbitMQBrokerParameters) Prelude.. Data._Sensitive

instance
  Data.FromJSON
    PipeSourceRabbitMQBrokerParameters
  where
  parseJSON =
    Data.withObject
      "PipeSourceRabbitMQBrokerParameters"
      ( \x ->
          PipeSourceRabbitMQBrokerParameters'
            Prelude.<$> (x Data..:? "BatchSize")
            Prelude.<*> (x Data..:? "MaximumBatchingWindowInSeconds")
            Prelude.<*> (x Data..:? "VirtualHost")
            Prelude.<*> (x Data..: "Credentials")
            Prelude.<*> (x Data..: "QueueName")
      )

instance
  Prelude.Hashable
    PipeSourceRabbitMQBrokerParameters
  where
  hashWithSalt
    _salt
    PipeSourceRabbitMQBrokerParameters' {..} =
      _salt
        `Prelude.hashWithSalt` batchSize
        `Prelude.hashWithSalt` maximumBatchingWindowInSeconds
        `Prelude.hashWithSalt` virtualHost
        `Prelude.hashWithSalt` credentials
        `Prelude.hashWithSalt` queueName

instance
  Prelude.NFData
    PipeSourceRabbitMQBrokerParameters
  where
  rnf PipeSourceRabbitMQBrokerParameters' {..} =
    Prelude.rnf batchSize
      `Prelude.seq` Prelude.rnf maximumBatchingWindowInSeconds
      `Prelude.seq` Prelude.rnf virtualHost
      `Prelude.seq` Prelude.rnf credentials
      `Prelude.seq` Prelude.rnf queueName

instance
  Data.ToJSON
    PipeSourceRabbitMQBrokerParameters
  where
  toJSON PipeSourceRabbitMQBrokerParameters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BatchSize" Data..=) Prelude.<$> batchSize,
            ("MaximumBatchingWindowInSeconds" Data..=)
              Prelude.<$> maximumBatchingWindowInSeconds,
            ("VirtualHost" Data..=) Prelude.<$> virtualHost,
            Prelude.Just ("Credentials" Data..= credentials),
            Prelude.Just ("QueueName" Data..= queueName)
          ]
      )

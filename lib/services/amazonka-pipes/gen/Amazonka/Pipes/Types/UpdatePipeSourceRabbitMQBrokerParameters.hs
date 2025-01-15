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
-- Module      : Amazonka.Pipes.Types.UpdatePipeSourceRabbitMQBrokerParameters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pipes.Types.UpdatePipeSourceRabbitMQBrokerParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pipes.Types.MQBrokerAccessCredentials
import qualified Amazonka.Prelude as Prelude

-- | The parameters for using a Rabbit MQ broker as a source.
--
-- /See:/ 'newUpdatePipeSourceRabbitMQBrokerParameters' smart constructor.
data UpdatePipeSourceRabbitMQBrokerParameters = UpdatePipeSourceRabbitMQBrokerParameters'
  { -- | The maximum number of records to include in each batch.
    batchSize :: Prelude.Maybe Prelude.Natural,
    -- | The maximum length of a time to wait for events.
    maximumBatchingWindowInSeconds :: Prelude.Maybe Prelude.Natural,
    -- | The credentials needed to access the resource.
    credentials :: MQBrokerAccessCredentials
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdatePipeSourceRabbitMQBrokerParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'batchSize', 'updatePipeSourceRabbitMQBrokerParameters_batchSize' - The maximum number of records to include in each batch.
--
-- 'maximumBatchingWindowInSeconds', 'updatePipeSourceRabbitMQBrokerParameters_maximumBatchingWindowInSeconds' - The maximum length of a time to wait for events.
--
-- 'credentials', 'updatePipeSourceRabbitMQBrokerParameters_credentials' - The credentials needed to access the resource.
newUpdatePipeSourceRabbitMQBrokerParameters ::
  -- | 'credentials'
  MQBrokerAccessCredentials ->
  UpdatePipeSourceRabbitMQBrokerParameters
newUpdatePipeSourceRabbitMQBrokerParameters
  pCredentials_ =
    UpdatePipeSourceRabbitMQBrokerParameters'
      { batchSize =
          Prelude.Nothing,
        maximumBatchingWindowInSeconds =
          Prelude.Nothing,
        credentials = pCredentials_
      }

-- | The maximum number of records to include in each batch.
updatePipeSourceRabbitMQBrokerParameters_batchSize :: Lens.Lens' UpdatePipeSourceRabbitMQBrokerParameters (Prelude.Maybe Prelude.Natural)
updatePipeSourceRabbitMQBrokerParameters_batchSize = Lens.lens (\UpdatePipeSourceRabbitMQBrokerParameters' {batchSize} -> batchSize) (\s@UpdatePipeSourceRabbitMQBrokerParameters' {} a -> s {batchSize = a} :: UpdatePipeSourceRabbitMQBrokerParameters)

-- | The maximum length of a time to wait for events.
updatePipeSourceRabbitMQBrokerParameters_maximumBatchingWindowInSeconds :: Lens.Lens' UpdatePipeSourceRabbitMQBrokerParameters (Prelude.Maybe Prelude.Natural)
updatePipeSourceRabbitMQBrokerParameters_maximumBatchingWindowInSeconds = Lens.lens (\UpdatePipeSourceRabbitMQBrokerParameters' {maximumBatchingWindowInSeconds} -> maximumBatchingWindowInSeconds) (\s@UpdatePipeSourceRabbitMQBrokerParameters' {} a -> s {maximumBatchingWindowInSeconds = a} :: UpdatePipeSourceRabbitMQBrokerParameters)

-- | The credentials needed to access the resource.
updatePipeSourceRabbitMQBrokerParameters_credentials :: Lens.Lens' UpdatePipeSourceRabbitMQBrokerParameters MQBrokerAccessCredentials
updatePipeSourceRabbitMQBrokerParameters_credentials = Lens.lens (\UpdatePipeSourceRabbitMQBrokerParameters' {credentials} -> credentials) (\s@UpdatePipeSourceRabbitMQBrokerParameters' {} a -> s {credentials = a} :: UpdatePipeSourceRabbitMQBrokerParameters)

instance
  Prelude.Hashable
    UpdatePipeSourceRabbitMQBrokerParameters
  where
  hashWithSalt
    _salt
    UpdatePipeSourceRabbitMQBrokerParameters' {..} =
      _salt
        `Prelude.hashWithSalt` batchSize
        `Prelude.hashWithSalt` maximumBatchingWindowInSeconds
        `Prelude.hashWithSalt` credentials

instance
  Prelude.NFData
    UpdatePipeSourceRabbitMQBrokerParameters
  where
  rnf UpdatePipeSourceRabbitMQBrokerParameters' {..} =
    Prelude.rnf batchSize `Prelude.seq`
      Prelude.rnf maximumBatchingWindowInSeconds `Prelude.seq`
        Prelude.rnf credentials

instance
  Data.ToJSON
    UpdatePipeSourceRabbitMQBrokerParameters
  where
  toJSON UpdatePipeSourceRabbitMQBrokerParameters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BatchSize" Data..=) Prelude.<$> batchSize,
            ("MaximumBatchingWindowInSeconds" Data..=)
              Prelude.<$> maximumBatchingWindowInSeconds,
            Prelude.Just ("Credentials" Data..= credentials)
          ]
      )

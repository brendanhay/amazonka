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
-- Module      : Amazonka.Pipes.Types.UpdatePipeSourceActiveMQBrokerParameters
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pipes.Types.UpdatePipeSourceActiveMQBrokerParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pipes.Types.MQBrokerAccessCredentials
import qualified Amazonka.Prelude as Prelude

-- | The parameters for using an Active MQ broker as a source.
--
-- /See:/ 'newUpdatePipeSourceActiveMQBrokerParameters' smart constructor.
data UpdatePipeSourceActiveMQBrokerParameters = UpdatePipeSourceActiveMQBrokerParameters'
  { -- | The maximum number of records to include in each batch.
    batchSize :: Prelude.Maybe Prelude.Natural,
    -- | The maximum length of a time to wait for events.
    maximumBatchingWindowInSeconds :: Prelude.Maybe Prelude.Natural,
    -- | The credentials needed to access the resource.
    credentials :: MQBrokerAccessCredentials
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdatePipeSourceActiveMQBrokerParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'batchSize', 'updatePipeSourceActiveMQBrokerParameters_batchSize' - The maximum number of records to include in each batch.
--
-- 'maximumBatchingWindowInSeconds', 'updatePipeSourceActiveMQBrokerParameters_maximumBatchingWindowInSeconds' - The maximum length of a time to wait for events.
--
-- 'credentials', 'updatePipeSourceActiveMQBrokerParameters_credentials' - The credentials needed to access the resource.
newUpdatePipeSourceActiveMQBrokerParameters ::
  -- | 'credentials'
  MQBrokerAccessCredentials ->
  UpdatePipeSourceActiveMQBrokerParameters
newUpdatePipeSourceActiveMQBrokerParameters
  pCredentials_ =
    UpdatePipeSourceActiveMQBrokerParameters'
      { batchSize =
          Prelude.Nothing,
        maximumBatchingWindowInSeconds =
          Prelude.Nothing,
        credentials = pCredentials_
      }

-- | The maximum number of records to include in each batch.
updatePipeSourceActiveMQBrokerParameters_batchSize :: Lens.Lens' UpdatePipeSourceActiveMQBrokerParameters (Prelude.Maybe Prelude.Natural)
updatePipeSourceActiveMQBrokerParameters_batchSize = Lens.lens (\UpdatePipeSourceActiveMQBrokerParameters' {batchSize} -> batchSize) (\s@UpdatePipeSourceActiveMQBrokerParameters' {} a -> s {batchSize = a} :: UpdatePipeSourceActiveMQBrokerParameters)

-- | The maximum length of a time to wait for events.
updatePipeSourceActiveMQBrokerParameters_maximumBatchingWindowInSeconds :: Lens.Lens' UpdatePipeSourceActiveMQBrokerParameters (Prelude.Maybe Prelude.Natural)
updatePipeSourceActiveMQBrokerParameters_maximumBatchingWindowInSeconds = Lens.lens (\UpdatePipeSourceActiveMQBrokerParameters' {maximumBatchingWindowInSeconds} -> maximumBatchingWindowInSeconds) (\s@UpdatePipeSourceActiveMQBrokerParameters' {} a -> s {maximumBatchingWindowInSeconds = a} :: UpdatePipeSourceActiveMQBrokerParameters)

-- | The credentials needed to access the resource.
updatePipeSourceActiveMQBrokerParameters_credentials :: Lens.Lens' UpdatePipeSourceActiveMQBrokerParameters MQBrokerAccessCredentials
updatePipeSourceActiveMQBrokerParameters_credentials = Lens.lens (\UpdatePipeSourceActiveMQBrokerParameters' {credentials} -> credentials) (\s@UpdatePipeSourceActiveMQBrokerParameters' {} a -> s {credentials = a} :: UpdatePipeSourceActiveMQBrokerParameters)

instance
  Prelude.Hashable
    UpdatePipeSourceActiveMQBrokerParameters
  where
  hashWithSalt
    _salt
    UpdatePipeSourceActiveMQBrokerParameters' {..} =
      _salt `Prelude.hashWithSalt` batchSize
        `Prelude.hashWithSalt` maximumBatchingWindowInSeconds
        `Prelude.hashWithSalt` credentials

instance
  Prelude.NFData
    UpdatePipeSourceActiveMQBrokerParameters
  where
  rnf UpdatePipeSourceActiveMQBrokerParameters' {..} =
    Prelude.rnf batchSize
      `Prelude.seq` Prelude.rnf maximumBatchingWindowInSeconds
      `Prelude.seq` Prelude.rnf credentials

instance
  Data.ToJSON
    UpdatePipeSourceActiveMQBrokerParameters
  where
  toJSON UpdatePipeSourceActiveMQBrokerParameters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BatchSize" Data..=) Prelude.<$> batchSize,
            ("MaximumBatchingWindowInSeconds" Data..=)
              Prelude.<$> maximumBatchingWindowInSeconds,
            Prelude.Just ("Credentials" Data..= credentials)
          ]
      )

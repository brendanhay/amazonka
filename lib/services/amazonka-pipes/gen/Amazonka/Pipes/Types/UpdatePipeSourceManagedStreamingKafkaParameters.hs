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
-- Module      : Amazonka.Pipes.Types.UpdatePipeSourceManagedStreamingKafkaParameters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pipes.Types.UpdatePipeSourceManagedStreamingKafkaParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pipes.Types.MSKAccessCredentials
import qualified Amazonka.Prelude as Prelude

-- | The parameters for using an MSK stream as a source.
--
-- /See:/ 'newUpdatePipeSourceManagedStreamingKafkaParameters' smart constructor.
data UpdatePipeSourceManagedStreamingKafkaParameters = UpdatePipeSourceManagedStreamingKafkaParameters'
  { -- | The maximum number of records to include in each batch.
    batchSize :: Prelude.Maybe Prelude.Natural,
    -- | The credentials needed to access the resource.
    credentials :: Prelude.Maybe MSKAccessCredentials,
    -- | The maximum length of a time to wait for events.
    maximumBatchingWindowInSeconds :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdatePipeSourceManagedStreamingKafkaParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'batchSize', 'updatePipeSourceManagedStreamingKafkaParameters_batchSize' - The maximum number of records to include in each batch.
--
-- 'credentials', 'updatePipeSourceManagedStreamingKafkaParameters_credentials' - The credentials needed to access the resource.
--
-- 'maximumBatchingWindowInSeconds', 'updatePipeSourceManagedStreamingKafkaParameters_maximumBatchingWindowInSeconds' - The maximum length of a time to wait for events.
newUpdatePipeSourceManagedStreamingKafkaParameters ::
  UpdatePipeSourceManagedStreamingKafkaParameters
newUpdatePipeSourceManagedStreamingKafkaParameters =
  UpdatePipeSourceManagedStreamingKafkaParameters'
    { batchSize =
        Prelude.Nothing,
      credentials =
        Prelude.Nothing,
      maximumBatchingWindowInSeconds =
        Prelude.Nothing
    }

-- | The maximum number of records to include in each batch.
updatePipeSourceManagedStreamingKafkaParameters_batchSize :: Lens.Lens' UpdatePipeSourceManagedStreamingKafkaParameters (Prelude.Maybe Prelude.Natural)
updatePipeSourceManagedStreamingKafkaParameters_batchSize = Lens.lens (\UpdatePipeSourceManagedStreamingKafkaParameters' {batchSize} -> batchSize) (\s@UpdatePipeSourceManagedStreamingKafkaParameters' {} a -> s {batchSize = a} :: UpdatePipeSourceManagedStreamingKafkaParameters)

-- | The credentials needed to access the resource.
updatePipeSourceManagedStreamingKafkaParameters_credentials :: Lens.Lens' UpdatePipeSourceManagedStreamingKafkaParameters (Prelude.Maybe MSKAccessCredentials)
updatePipeSourceManagedStreamingKafkaParameters_credentials = Lens.lens (\UpdatePipeSourceManagedStreamingKafkaParameters' {credentials} -> credentials) (\s@UpdatePipeSourceManagedStreamingKafkaParameters' {} a -> s {credentials = a} :: UpdatePipeSourceManagedStreamingKafkaParameters)

-- | The maximum length of a time to wait for events.
updatePipeSourceManagedStreamingKafkaParameters_maximumBatchingWindowInSeconds :: Lens.Lens' UpdatePipeSourceManagedStreamingKafkaParameters (Prelude.Maybe Prelude.Natural)
updatePipeSourceManagedStreamingKafkaParameters_maximumBatchingWindowInSeconds = Lens.lens (\UpdatePipeSourceManagedStreamingKafkaParameters' {maximumBatchingWindowInSeconds} -> maximumBatchingWindowInSeconds) (\s@UpdatePipeSourceManagedStreamingKafkaParameters' {} a -> s {maximumBatchingWindowInSeconds = a} :: UpdatePipeSourceManagedStreamingKafkaParameters)

instance
  Prelude.Hashable
    UpdatePipeSourceManagedStreamingKafkaParameters
  where
  hashWithSalt
    _salt
    UpdatePipeSourceManagedStreamingKafkaParameters' {..} =
      _salt `Prelude.hashWithSalt` batchSize
        `Prelude.hashWithSalt` credentials
        `Prelude.hashWithSalt` maximumBatchingWindowInSeconds

instance
  Prelude.NFData
    UpdatePipeSourceManagedStreamingKafkaParameters
  where
  rnf
    UpdatePipeSourceManagedStreamingKafkaParameters' {..} =
      Prelude.rnf batchSize
        `Prelude.seq` Prelude.rnf credentials
        `Prelude.seq` Prelude.rnf maximumBatchingWindowInSeconds

instance
  Data.ToJSON
    UpdatePipeSourceManagedStreamingKafkaParameters
  where
  toJSON
    UpdatePipeSourceManagedStreamingKafkaParameters' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("BatchSize" Data..=) Prelude.<$> batchSize,
              ("Credentials" Data..=) Prelude.<$> credentials,
              ("MaximumBatchingWindowInSeconds" Data..=)
                Prelude.<$> maximumBatchingWindowInSeconds
            ]
        )

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
-- Module      : Amazonka.Location.Types.BatchGetDevicePositionError
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Location.Types.BatchGetDevicePositionError where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Location.Types.BatchItemError
import qualified Amazonka.Prelude as Prelude

-- | Contains error details for each device that didn\'t return a position.
--
-- /See:/ 'newBatchGetDevicePositionError' smart constructor.
data BatchGetDevicePositionError = BatchGetDevicePositionError'
  { -- | The ID of the device that didn\'t return a position.
    deviceId :: Prelude.Text,
    -- | Contains details related to the error code.
    error :: BatchItemError
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetDevicePositionError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceId', 'batchGetDevicePositionError_deviceId' - The ID of the device that didn\'t return a position.
--
-- 'error', 'batchGetDevicePositionError_error' - Contains details related to the error code.
newBatchGetDevicePositionError ::
  -- | 'deviceId'
  Prelude.Text ->
  -- | 'error'
  BatchItemError ->
  BatchGetDevicePositionError
newBatchGetDevicePositionError pDeviceId_ pError_ =
  BatchGetDevicePositionError'
    { deviceId = pDeviceId_,
      error = pError_
    }

-- | The ID of the device that didn\'t return a position.
batchGetDevicePositionError_deviceId :: Lens.Lens' BatchGetDevicePositionError Prelude.Text
batchGetDevicePositionError_deviceId = Lens.lens (\BatchGetDevicePositionError' {deviceId} -> deviceId) (\s@BatchGetDevicePositionError' {} a -> s {deviceId = a} :: BatchGetDevicePositionError)

-- | Contains details related to the error code.
batchGetDevicePositionError_error :: Lens.Lens' BatchGetDevicePositionError BatchItemError
batchGetDevicePositionError_error = Lens.lens (\BatchGetDevicePositionError' {error} -> error) (\s@BatchGetDevicePositionError' {} a -> s {error = a} :: BatchGetDevicePositionError)

instance Data.FromJSON BatchGetDevicePositionError where
  parseJSON =
    Data.withObject
      "BatchGetDevicePositionError"
      ( \x ->
          BatchGetDevicePositionError'
            Prelude.<$> (x Data..: "DeviceId")
            Prelude.<*> (x Data..: "Error")
      )

instance Prelude.Hashable BatchGetDevicePositionError where
  hashWithSalt _salt BatchGetDevicePositionError' {..} =
    _salt
      `Prelude.hashWithSalt` deviceId
      `Prelude.hashWithSalt` error

instance Prelude.NFData BatchGetDevicePositionError where
  rnf BatchGetDevicePositionError' {..} =
    Prelude.rnf deviceId `Prelude.seq`
      Prelude.rnf error

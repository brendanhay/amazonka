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
-- Module      : Amazonka.Location.Types.BatchDeleteDevicePositionHistoryError
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Location.Types.BatchDeleteDevicePositionHistoryError where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Location.Types.BatchItemError
import qualified Amazonka.Prelude as Prelude

-- | Contains the tracker resource details.
--
-- /See:/ 'newBatchDeleteDevicePositionHistoryError' smart constructor.
data BatchDeleteDevicePositionHistoryError = BatchDeleteDevicePositionHistoryError'
  { -- | The ID of the device for this position.
    deviceId :: Prelude.Text,
    error :: BatchItemError
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchDeleteDevicePositionHistoryError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceId', 'batchDeleteDevicePositionHistoryError_deviceId' - The ID of the device for this position.
--
-- 'error', 'batchDeleteDevicePositionHistoryError_error' - Undocumented member.
newBatchDeleteDevicePositionHistoryError ::
  -- | 'deviceId'
  Prelude.Text ->
  -- | 'error'
  BatchItemError ->
  BatchDeleteDevicePositionHistoryError
newBatchDeleteDevicePositionHistoryError
  pDeviceId_
  pError_ =
    BatchDeleteDevicePositionHistoryError'
      { deviceId =
          pDeviceId_,
        error = pError_
      }

-- | The ID of the device for this position.
batchDeleteDevicePositionHistoryError_deviceId :: Lens.Lens' BatchDeleteDevicePositionHistoryError Prelude.Text
batchDeleteDevicePositionHistoryError_deviceId = Lens.lens (\BatchDeleteDevicePositionHistoryError' {deviceId} -> deviceId) (\s@BatchDeleteDevicePositionHistoryError' {} a -> s {deviceId = a} :: BatchDeleteDevicePositionHistoryError)

-- | Undocumented member.
batchDeleteDevicePositionHistoryError_error :: Lens.Lens' BatchDeleteDevicePositionHistoryError BatchItemError
batchDeleteDevicePositionHistoryError_error = Lens.lens (\BatchDeleteDevicePositionHistoryError' {error} -> error) (\s@BatchDeleteDevicePositionHistoryError' {} a -> s {error = a} :: BatchDeleteDevicePositionHistoryError)

instance
  Data.FromJSON
    BatchDeleteDevicePositionHistoryError
  where
  parseJSON =
    Data.withObject
      "BatchDeleteDevicePositionHistoryError"
      ( \x ->
          BatchDeleteDevicePositionHistoryError'
            Prelude.<$> (x Data..: "DeviceId")
            Prelude.<*> (x Data..: "Error")
      )

instance
  Prelude.Hashable
    BatchDeleteDevicePositionHistoryError
  where
  hashWithSalt
    _salt
    BatchDeleteDevicePositionHistoryError' {..} =
      _salt `Prelude.hashWithSalt` deviceId
        `Prelude.hashWithSalt` error

instance
  Prelude.NFData
    BatchDeleteDevicePositionHistoryError
  where
  rnf BatchDeleteDevicePositionHistoryError' {..} =
    Prelude.rnf deviceId
      `Prelude.seq` Prelude.rnf error

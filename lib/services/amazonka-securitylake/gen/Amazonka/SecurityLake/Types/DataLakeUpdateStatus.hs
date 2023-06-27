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
-- Module      : Amazonka.SecurityLake.Types.DataLakeUpdateStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityLake.Types.DataLakeUpdateStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityLake.Types.DataLakeStatus
import Amazonka.SecurityLake.Types.DataLakeUpdateException

-- | The status of the last @UpdateDataLake@ or @DeleteDataLake@ API request.
-- This is set to Completed after the configuration is updated, or removed
-- if deletion of the data lake is successful.
--
-- /See:/ 'newDataLakeUpdateStatus' smart constructor.
data DataLakeUpdateStatus = DataLakeUpdateStatus'
  { -- | The details of the last @UpdateDataLake@or @DeleteDataLake@ API request
    -- which failed.
    exception :: Prelude.Maybe DataLakeUpdateException,
    -- | The unique ID for the last @UpdateDataLake@ or @DeleteDataLake@ API
    -- request.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The status of the last @UpdateDataLake@ or @DeleteDataLake@ API request
    -- that was requested.
    status :: Prelude.Maybe DataLakeStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataLakeUpdateStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'exception', 'dataLakeUpdateStatus_exception' - The details of the last @UpdateDataLake@or @DeleteDataLake@ API request
-- which failed.
--
-- 'requestId', 'dataLakeUpdateStatus_requestId' - The unique ID for the last @UpdateDataLake@ or @DeleteDataLake@ API
-- request.
--
-- 'status', 'dataLakeUpdateStatus_status' - The status of the last @UpdateDataLake@ or @DeleteDataLake@ API request
-- that was requested.
newDataLakeUpdateStatus ::
  DataLakeUpdateStatus
newDataLakeUpdateStatus =
  DataLakeUpdateStatus'
    { exception = Prelude.Nothing,
      requestId = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The details of the last @UpdateDataLake@or @DeleteDataLake@ API request
-- which failed.
dataLakeUpdateStatus_exception :: Lens.Lens' DataLakeUpdateStatus (Prelude.Maybe DataLakeUpdateException)
dataLakeUpdateStatus_exception = Lens.lens (\DataLakeUpdateStatus' {exception} -> exception) (\s@DataLakeUpdateStatus' {} a -> s {exception = a} :: DataLakeUpdateStatus)

-- | The unique ID for the last @UpdateDataLake@ or @DeleteDataLake@ API
-- request.
dataLakeUpdateStatus_requestId :: Lens.Lens' DataLakeUpdateStatus (Prelude.Maybe Prelude.Text)
dataLakeUpdateStatus_requestId = Lens.lens (\DataLakeUpdateStatus' {requestId} -> requestId) (\s@DataLakeUpdateStatus' {} a -> s {requestId = a} :: DataLakeUpdateStatus)

-- | The status of the last @UpdateDataLake@ or @DeleteDataLake@ API request
-- that was requested.
dataLakeUpdateStatus_status :: Lens.Lens' DataLakeUpdateStatus (Prelude.Maybe DataLakeStatus)
dataLakeUpdateStatus_status = Lens.lens (\DataLakeUpdateStatus' {status} -> status) (\s@DataLakeUpdateStatus' {} a -> s {status = a} :: DataLakeUpdateStatus)

instance Data.FromJSON DataLakeUpdateStatus where
  parseJSON =
    Data.withObject
      "DataLakeUpdateStatus"
      ( \x ->
          DataLakeUpdateStatus'
            Prelude.<$> (x Data..:? "exception")
            Prelude.<*> (x Data..:? "requestId")
            Prelude.<*> (x Data..:? "status")
      )

instance Prelude.Hashable DataLakeUpdateStatus where
  hashWithSalt _salt DataLakeUpdateStatus' {..} =
    _salt
      `Prelude.hashWithSalt` exception
      `Prelude.hashWithSalt` requestId
      `Prelude.hashWithSalt` status

instance Prelude.NFData DataLakeUpdateStatus where
  rnf DataLakeUpdateStatus' {..} =
    Prelude.rnf exception
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf status

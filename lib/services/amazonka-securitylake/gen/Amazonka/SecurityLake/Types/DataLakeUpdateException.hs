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
-- Module      : Amazonka.SecurityLake.Types.DataLakeUpdateException
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityLake.Types.DataLakeUpdateException where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The details of the last @UpdateDataLake@ or @DeleteDataLake@ API request
-- which failed.
--
-- /See:/ 'newDataLakeUpdateException' smart constructor.
data DataLakeUpdateException = DataLakeUpdateException'
  { -- | The reason code for the exception of the last @UpdateDataLake@ or
    -- @DeleteDataLake@ API request.
    code :: Prelude.Maybe Prelude.Text,
    -- | The reason for the exception of the last @UpdateDataLake@or
    -- @DeleteDataLake@ API request.
    reason :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataLakeUpdateException' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'code', 'dataLakeUpdateException_code' - The reason code for the exception of the last @UpdateDataLake@ or
-- @DeleteDataLake@ API request.
--
-- 'reason', 'dataLakeUpdateException_reason' - The reason for the exception of the last @UpdateDataLake@or
-- @DeleteDataLake@ API request.
newDataLakeUpdateException ::
  DataLakeUpdateException
newDataLakeUpdateException =
  DataLakeUpdateException'
    { code = Prelude.Nothing,
      reason = Prelude.Nothing
    }

-- | The reason code for the exception of the last @UpdateDataLake@ or
-- @DeleteDataLake@ API request.
dataLakeUpdateException_code :: Lens.Lens' DataLakeUpdateException (Prelude.Maybe Prelude.Text)
dataLakeUpdateException_code = Lens.lens (\DataLakeUpdateException' {code} -> code) (\s@DataLakeUpdateException' {} a -> s {code = a} :: DataLakeUpdateException)

-- | The reason for the exception of the last @UpdateDataLake@or
-- @DeleteDataLake@ API request.
dataLakeUpdateException_reason :: Lens.Lens' DataLakeUpdateException (Prelude.Maybe Prelude.Text)
dataLakeUpdateException_reason = Lens.lens (\DataLakeUpdateException' {reason} -> reason) (\s@DataLakeUpdateException' {} a -> s {reason = a} :: DataLakeUpdateException)

instance Data.FromJSON DataLakeUpdateException where
  parseJSON =
    Data.withObject
      "DataLakeUpdateException"
      ( \x ->
          DataLakeUpdateException'
            Prelude.<$> (x Data..:? "code")
            Prelude.<*> (x Data..:? "reason")
      )

instance Prelude.Hashable DataLakeUpdateException where
  hashWithSalt _salt DataLakeUpdateException' {..} =
    _salt
      `Prelude.hashWithSalt` code
      `Prelude.hashWithSalt` reason

instance Prelude.NFData DataLakeUpdateException where
  rnf DataLakeUpdateException' {..} =
    Prelude.rnf code `Prelude.seq` Prelude.rnf reason

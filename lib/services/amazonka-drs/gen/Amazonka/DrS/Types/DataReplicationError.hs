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
-- Module      : Amazonka.DrS.Types.DataReplicationError
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DrS.Types.DataReplicationError where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DrS.Types.DataReplicationErrorString
import qualified Amazonka.Prelude as Prelude

-- | Error in data replication.
--
-- /See:/ 'newDataReplicationError' smart constructor.
data DataReplicationError = DataReplicationError'
  { -- | Error in data replication.
    error :: Prelude.Maybe DataReplicationErrorString,
    -- | Error in data replication.
    rawError :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataReplicationError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'error', 'dataReplicationError_error' - Error in data replication.
--
-- 'rawError', 'dataReplicationError_rawError' - Error in data replication.
newDataReplicationError ::
  DataReplicationError
newDataReplicationError =
  DataReplicationError'
    { error = Prelude.Nothing,
      rawError = Prelude.Nothing
    }

-- | Error in data replication.
dataReplicationError_error :: Lens.Lens' DataReplicationError (Prelude.Maybe DataReplicationErrorString)
dataReplicationError_error = Lens.lens (\DataReplicationError' {error} -> error) (\s@DataReplicationError' {} a -> s {error = a} :: DataReplicationError)

-- | Error in data replication.
dataReplicationError_rawError :: Lens.Lens' DataReplicationError (Prelude.Maybe Prelude.Text)
dataReplicationError_rawError = Lens.lens (\DataReplicationError' {rawError} -> rawError) (\s@DataReplicationError' {} a -> s {rawError = a} :: DataReplicationError)

instance Data.FromJSON DataReplicationError where
  parseJSON =
    Data.withObject
      "DataReplicationError"
      ( \x ->
          DataReplicationError'
            Prelude.<$> (x Data..:? "error")
            Prelude.<*> (x Data..:? "rawError")
      )

instance Prelude.Hashable DataReplicationError where
  hashWithSalt _salt DataReplicationError' {..} =
    _salt
      `Prelude.hashWithSalt` error
      `Prelude.hashWithSalt` rawError

instance Prelude.NFData DataReplicationError where
  rnf DataReplicationError' {..} =
    Prelude.rnf error `Prelude.seq`
      Prelude.rnf rawError

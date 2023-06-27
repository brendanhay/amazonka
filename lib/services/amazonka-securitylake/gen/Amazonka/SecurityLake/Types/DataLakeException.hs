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
-- Module      : Amazonka.SecurityLake.Types.DataLakeException
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityLake.Types.DataLakeException where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The details for a Security Lake exception
--
-- /See:/ 'newDataLakeException' smart constructor.
data DataLakeException = DataLakeException'
  { -- | The underlying exception of a Security Lake exception.
    exception :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services Regions where the exception occurred.
    region :: Prelude.Maybe Prelude.Text,
    -- | List of all remediation steps for a Security Lake exception.
    remediation :: Prelude.Maybe Prelude.Text,
    -- | This error can occur if you configure the wrong timestamp format, or if
    -- the subset of entries used for validation had errors or missing values.
    timestamp :: Prelude.Maybe Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataLakeException' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'exception', 'dataLakeException_exception' - The underlying exception of a Security Lake exception.
--
-- 'region', 'dataLakeException_region' - The Amazon Web Services Regions where the exception occurred.
--
-- 'remediation', 'dataLakeException_remediation' - List of all remediation steps for a Security Lake exception.
--
-- 'timestamp', 'dataLakeException_timestamp' - This error can occur if you configure the wrong timestamp format, or if
-- the subset of entries used for validation had errors or missing values.
newDataLakeException ::
  DataLakeException
newDataLakeException =
  DataLakeException'
    { exception = Prelude.Nothing,
      region = Prelude.Nothing,
      remediation = Prelude.Nothing,
      timestamp = Prelude.Nothing
    }

-- | The underlying exception of a Security Lake exception.
dataLakeException_exception :: Lens.Lens' DataLakeException (Prelude.Maybe Prelude.Text)
dataLakeException_exception = Lens.lens (\DataLakeException' {exception} -> exception) (\s@DataLakeException' {} a -> s {exception = a} :: DataLakeException)

-- | The Amazon Web Services Regions where the exception occurred.
dataLakeException_region :: Lens.Lens' DataLakeException (Prelude.Maybe Prelude.Text)
dataLakeException_region = Lens.lens (\DataLakeException' {region} -> region) (\s@DataLakeException' {} a -> s {region = a} :: DataLakeException)

-- | List of all remediation steps for a Security Lake exception.
dataLakeException_remediation :: Lens.Lens' DataLakeException (Prelude.Maybe Prelude.Text)
dataLakeException_remediation = Lens.lens (\DataLakeException' {remediation} -> remediation) (\s@DataLakeException' {} a -> s {remediation = a} :: DataLakeException)

-- | This error can occur if you configure the wrong timestamp format, or if
-- the subset of entries used for validation had errors or missing values.
dataLakeException_timestamp :: Lens.Lens' DataLakeException (Prelude.Maybe Prelude.UTCTime)
dataLakeException_timestamp = Lens.lens (\DataLakeException' {timestamp} -> timestamp) (\s@DataLakeException' {} a -> s {timestamp = a} :: DataLakeException) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON DataLakeException where
  parseJSON =
    Data.withObject
      "DataLakeException"
      ( \x ->
          DataLakeException'
            Prelude.<$> (x Data..:? "exception")
            Prelude.<*> (x Data..:? "region")
            Prelude.<*> (x Data..:? "remediation")
            Prelude.<*> (x Data..:? "timestamp")
      )

instance Prelude.Hashable DataLakeException where
  hashWithSalt _salt DataLakeException' {..} =
    _salt
      `Prelude.hashWithSalt` exception
      `Prelude.hashWithSalt` region
      `Prelude.hashWithSalt` remediation
      `Prelude.hashWithSalt` timestamp

instance Prelude.NFData DataLakeException where
  rnf DataLakeException' {..} =
    Prelude.rnf exception
      `Prelude.seq` Prelude.rnf region
      `Prelude.seq` Prelude.rnf remediation
      `Prelude.seq` Prelude.rnf timestamp

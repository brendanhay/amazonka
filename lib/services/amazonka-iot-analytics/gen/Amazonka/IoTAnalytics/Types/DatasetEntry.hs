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
-- Module      : Amazonka.IoTAnalytics.Types.DatasetEntry
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTAnalytics.Types.DatasetEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The reference to a dataset entry.
--
-- /See:/ 'newDatasetEntry' smart constructor.
data DatasetEntry = DatasetEntry'
  { -- | The presigned URI of the dataset item.
    dataURI :: Prelude.Maybe Prelude.Text,
    -- | The name of the dataset item.
    entryName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DatasetEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataURI', 'datasetEntry_dataURI' - The presigned URI of the dataset item.
--
-- 'entryName', 'datasetEntry_entryName' - The name of the dataset item.
newDatasetEntry ::
  DatasetEntry
newDatasetEntry =
  DatasetEntry'
    { dataURI = Prelude.Nothing,
      entryName = Prelude.Nothing
    }

-- | The presigned URI of the dataset item.
datasetEntry_dataURI :: Lens.Lens' DatasetEntry (Prelude.Maybe Prelude.Text)
datasetEntry_dataURI = Lens.lens (\DatasetEntry' {dataURI} -> dataURI) (\s@DatasetEntry' {} a -> s {dataURI = a} :: DatasetEntry)

-- | The name of the dataset item.
datasetEntry_entryName :: Lens.Lens' DatasetEntry (Prelude.Maybe Prelude.Text)
datasetEntry_entryName = Lens.lens (\DatasetEntry' {entryName} -> entryName) (\s@DatasetEntry' {} a -> s {entryName = a} :: DatasetEntry)

instance Data.FromJSON DatasetEntry where
  parseJSON =
    Data.withObject
      "DatasetEntry"
      ( \x ->
          DatasetEntry'
            Prelude.<$> (x Data..:? "dataURI")
            Prelude.<*> (x Data..:? "entryName")
      )

instance Prelude.Hashable DatasetEntry where
  hashWithSalt _salt DatasetEntry' {..} =
    _salt
      `Prelude.hashWithSalt` dataURI
      `Prelude.hashWithSalt` entryName

instance Prelude.NFData DatasetEntry where
  rnf DatasetEntry' {..} =
    Prelude.rnf dataURI `Prelude.seq`
      Prelude.rnf entryName

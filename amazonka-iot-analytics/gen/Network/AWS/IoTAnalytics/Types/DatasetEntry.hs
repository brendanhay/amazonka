{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.IoTAnalytics.Types.DatasetEntry
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.DatasetEntry where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The reference to a data set entry.
--
-- /See:/ 'newDatasetEntry' smart constructor.
data DatasetEntry = DatasetEntry'
  { -- | The presigned URI of the data set item.
    dataURI :: Prelude.Maybe Prelude.Text,
    -- | The name of the data set item.
    entryName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DatasetEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataURI', 'datasetEntry_dataURI' - The presigned URI of the data set item.
--
-- 'entryName', 'datasetEntry_entryName' - The name of the data set item.
newDatasetEntry ::
  DatasetEntry
newDatasetEntry =
  DatasetEntry'
    { dataURI = Prelude.Nothing,
      entryName = Prelude.Nothing
    }

-- | The presigned URI of the data set item.
datasetEntry_dataURI :: Lens.Lens' DatasetEntry (Prelude.Maybe Prelude.Text)
datasetEntry_dataURI = Lens.lens (\DatasetEntry' {dataURI} -> dataURI) (\s@DatasetEntry' {} a -> s {dataURI = a} :: DatasetEntry)

-- | The name of the data set item.
datasetEntry_entryName :: Lens.Lens' DatasetEntry (Prelude.Maybe Prelude.Text)
datasetEntry_entryName = Lens.lens (\DatasetEntry' {entryName} -> entryName) (\s@DatasetEntry' {} a -> s {entryName = a} :: DatasetEntry)

instance Prelude.FromJSON DatasetEntry where
  parseJSON =
    Prelude.withObject
      "DatasetEntry"
      ( \x ->
          DatasetEntry'
            Prelude.<$> (x Prelude..:? "dataURI")
            Prelude.<*> (x Prelude..:? "entryName")
      )

instance Prelude.Hashable DatasetEntry

instance Prelude.NFData DatasetEntry

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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The reference to a data set entry.
--
-- /See:/ 'newDatasetEntry' smart constructor.
data DatasetEntry = DatasetEntry'
  { -- | The presigned URI of the data set item.
    dataURI :: Core.Maybe Core.Text,
    -- | The name of the data set item.
    entryName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { dataURI = Core.Nothing,
      entryName = Core.Nothing
    }

-- | The presigned URI of the data set item.
datasetEntry_dataURI :: Lens.Lens' DatasetEntry (Core.Maybe Core.Text)
datasetEntry_dataURI = Lens.lens (\DatasetEntry' {dataURI} -> dataURI) (\s@DatasetEntry' {} a -> s {dataURI = a} :: DatasetEntry)

-- | The name of the data set item.
datasetEntry_entryName :: Lens.Lens' DatasetEntry (Core.Maybe Core.Text)
datasetEntry_entryName = Lens.lens (\DatasetEntry' {entryName} -> entryName) (\s@DatasetEntry' {} a -> s {entryName = a} :: DatasetEntry)

instance Core.FromJSON DatasetEntry where
  parseJSON =
    Core.withObject
      "DatasetEntry"
      ( \x ->
          DatasetEntry'
            Core.<$> (x Core..:? "dataURI")
            Core.<*> (x Core..:? "entryName")
      )

instance Core.Hashable DatasetEntry

instance Core.NFData DatasetEntry

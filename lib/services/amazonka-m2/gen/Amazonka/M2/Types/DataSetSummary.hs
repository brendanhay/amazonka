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
-- Module      : Amazonka.M2.Types.DataSetSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.M2.Types.DataSetSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A subset of the possible data set attributes.
--
-- /See:/ 'newDataSetSummary' smart constructor.
data DataSetSummary = DataSetSummary'
  { -- | The format of the data set.
    format :: Prelude.Maybe Prelude.Text,
    -- | The last time the data set was updated.
    lastUpdatedTime :: Prelude.Maybe Core.POSIX,
    -- | The type of data set. Possible values include VSAM, IS, PS, GDG, PO, PS,
    -- or unknown.
    dataSetOrg :: Prelude.Maybe Prelude.Text,
    -- | The last time the data set was referenced.
    lastReferencedTime :: Prelude.Maybe Core.POSIX,
    -- | The timestamp when the data set was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | The name of the data set.
    dataSetName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataSetSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'format', 'dataSetSummary_format' - The format of the data set.
--
-- 'lastUpdatedTime', 'dataSetSummary_lastUpdatedTime' - The last time the data set was updated.
--
-- 'dataSetOrg', 'dataSetSummary_dataSetOrg' - The type of data set. Possible values include VSAM, IS, PS, GDG, PO, PS,
-- or unknown.
--
-- 'lastReferencedTime', 'dataSetSummary_lastReferencedTime' - The last time the data set was referenced.
--
-- 'creationTime', 'dataSetSummary_creationTime' - The timestamp when the data set was created.
--
-- 'dataSetName', 'dataSetSummary_dataSetName' - The name of the data set.
newDataSetSummary ::
  -- | 'dataSetName'
  Prelude.Text ->
  DataSetSummary
newDataSetSummary pDataSetName_ =
  DataSetSummary'
    { format = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      dataSetOrg = Prelude.Nothing,
      lastReferencedTime = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      dataSetName = pDataSetName_
    }

-- | The format of the data set.
dataSetSummary_format :: Lens.Lens' DataSetSummary (Prelude.Maybe Prelude.Text)
dataSetSummary_format = Lens.lens (\DataSetSummary' {format} -> format) (\s@DataSetSummary' {} a -> s {format = a} :: DataSetSummary)

-- | The last time the data set was updated.
dataSetSummary_lastUpdatedTime :: Lens.Lens' DataSetSummary (Prelude.Maybe Prelude.UTCTime)
dataSetSummary_lastUpdatedTime = Lens.lens (\DataSetSummary' {lastUpdatedTime} -> lastUpdatedTime) (\s@DataSetSummary' {} a -> s {lastUpdatedTime = a} :: DataSetSummary) Prelude.. Lens.mapping Core._Time

-- | The type of data set. Possible values include VSAM, IS, PS, GDG, PO, PS,
-- or unknown.
dataSetSummary_dataSetOrg :: Lens.Lens' DataSetSummary (Prelude.Maybe Prelude.Text)
dataSetSummary_dataSetOrg = Lens.lens (\DataSetSummary' {dataSetOrg} -> dataSetOrg) (\s@DataSetSummary' {} a -> s {dataSetOrg = a} :: DataSetSummary)

-- | The last time the data set was referenced.
dataSetSummary_lastReferencedTime :: Lens.Lens' DataSetSummary (Prelude.Maybe Prelude.UTCTime)
dataSetSummary_lastReferencedTime = Lens.lens (\DataSetSummary' {lastReferencedTime} -> lastReferencedTime) (\s@DataSetSummary' {} a -> s {lastReferencedTime = a} :: DataSetSummary) Prelude.. Lens.mapping Core._Time

-- | The timestamp when the data set was created.
dataSetSummary_creationTime :: Lens.Lens' DataSetSummary (Prelude.Maybe Prelude.UTCTime)
dataSetSummary_creationTime = Lens.lens (\DataSetSummary' {creationTime} -> creationTime) (\s@DataSetSummary' {} a -> s {creationTime = a} :: DataSetSummary) Prelude.. Lens.mapping Core._Time

-- | The name of the data set.
dataSetSummary_dataSetName :: Lens.Lens' DataSetSummary Prelude.Text
dataSetSummary_dataSetName = Lens.lens (\DataSetSummary' {dataSetName} -> dataSetName) (\s@DataSetSummary' {} a -> s {dataSetName = a} :: DataSetSummary)

instance Core.FromJSON DataSetSummary where
  parseJSON =
    Core.withObject
      "DataSetSummary"
      ( \x ->
          DataSetSummary'
            Prelude.<$> (x Core..:? "format")
            Prelude.<*> (x Core..:? "lastUpdatedTime")
            Prelude.<*> (x Core..:? "dataSetOrg")
            Prelude.<*> (x Core..:? "lastReferencedTime")
            Prelude.<*> (x Core..:? "creationTime")
            Prelude.<*> (x Core..: "dataSetName")
      )

instance Prelude.Hashable DataSetSummary where
  hashWithSalt _salt DataSetSummary' {..} =
    _salt `Prelude.hashWithSalt` format
      `Prelude.hashWithSalt` lastUpdatedTime
      `Prelude.hashWithSalt` dataSetOrg
      `Prelude.hashWithSalt` lastReferencedTime
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` dataSetName

instance Prelude.NFData DataSetSummary where
  rnf DataSetSummary' {..} =
    Prelude.rnf format
      `Prelude.seq` Prelude.rnf lastUpdatedTime
      `Prelude.seq` Prelude.rnf dataSetOrg
      `Prelude.seq` Prelude.rnf lastReferencedTime
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf dataSetName

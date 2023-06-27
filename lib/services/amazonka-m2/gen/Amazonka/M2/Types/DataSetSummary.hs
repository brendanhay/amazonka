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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.M2.Types.DataSetSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A subset of the possible data set attributes.
--
-- /See:/ 'newDataSetSummary' smart constructor.
data DataSetSummary = DataSetSummary'
  { -- | The timestamp when the data set was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The type of data set. The only supported value is VSAM.
    dataSetOrg :: Prelude.Maybe Prelude.Text,
    -- | The format of the data set.
    format :: Prelude.Maybe Prelude.Text,
    -- | The last time the data set was referenced.
    lastReferencedTime :: Prelude.Maybe Data.POSIX,
    -- | The last time the data set was updated.
    lastUpdatedTime :: Prelude.Maybe Data.POSIX,
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
-- 'creationTime', 'dataSetSummary_creationTime' - The timestamp when the data set was created.
--
-- 'dataSetOrg', 'dataSetSummary_dataSetOrg' - The type of data set. The only supported value is VSAM.
--
-- 'format', 'dataSetSummary_format' - The format of the data set.
--
-- 'lastReferencedTime', 'dataSetSummary_lastReferencedTime' - The last time the data set was referenced.
--
-- 'lastUpdatedTime', 'dataSetSummary_lastUpdatedTime' - The last time the data set was updated.
--
-- 'dataSetName', 'dataSetSummary_dataSetName' - The name of the data set.
newDataSetSummary ::
  -- | 'dataSetName'
  Prelude.Text ->
  DataSetSummary
newDataSetSummary pDataSetName_ =
  DataSetSummary'
    { creationTime = Prelude.Nothing,
      dataSetOrg = Prelude.Nothing,
      format = Prelude.Nothing,
      lastReferencedTime = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      dataSetName = pDataSetName_
    }

-- | The timestamp when the data set was created.
dataSetSummary_creationTime :: Lens.Lens' DataSetSummary (Prelude.Maybe Prelude.UTCTime)
dataSetSummary_creationTime = Lens.lens (\DataSetSummary' {creationTime} -> creationTime) (\s@DataSetSummary' {} a -> s {creationTime = a} :: DataSetSummary) Prelude.. Lens.mapping Data._Time

-- | The type of data set. The only supported value is VSAM.
dataSetSummary_dataSetOrg :: Lens.Lens' DataSetSummary (Prelude.Maybe Prelude.Text)
dataSetSummary_dataSetOrg = Lens.lens (\DataSetSummary' {dataSetOrg} -> dataSetOrg) (\s@DataSetSummary' {} a -> s {dataSetOrg = a} :: DataSetSummary)

-- | The format of the data set.
dataSetSummary_format :: Lens.Lens' DataSetSummary (Prelude.Maybe Prelude.Text)
dataSetSummary_format = Lens.lens (\DataSetSummary' {format} -> format) (\s@DataSetSummary' {} a -> s {format = a} :: DataSetSummary)

-- | The last time the data set was referenced.
dataSetSummary_lastReferencedTime :: Lens.Lens' DataSetSummary (Prelude.Maybe Prelude.UTCTime)
dataSetSummary_lastReferencedTime = Lens.lens (\DataSetSummary' {lastReferencedTime} -> lastReferencedTime) (\s@DataSetSummary' {} a -> s {lastReferencedTime = a} :: DataSetSummary) Prelude.. Lens.mapping Data._Time

-- | The last time the data set was updated.
dataSetSummary_lastUpdatedTime :: Lens.Lens' DataSetSummary (Prelude.Maybe Prelude.UTCTime)
dataSetSummary_lastUpdatedTime = Lens.lens (\DataSetSummary' {lastUpdatedTime} -> lastUpdatedTime) (\s@DataSetSummary' {} a -> s {lastUpdatedTime = a} :: DataSetSummary) Prelude.. Lens.mapping Data._Time

-- | The name of the data set.
dataSetSummary_dataSetName :: Lens.Lens' DataSetSummary Prelude.Text
dataSetSummary_dataSetName = Lens.lens (\DataSetSummary' {dataSetName} -> dataSetName) (\s@DataSetSummary' {} a -> s {dataSetName = a} :: DataSetSummary)

instance Data.FromJSON DataSetSummary where
  parseJSON =
    Data.withObject
      "DataSetSummary"
      ( \x ->
          DataSetSummary'
            Prelude.<$> (x Data..:? "creationTime")
            Prelude.<*> (x Data..:? "dataSetOrg")
            Prelude.<*> (x Data..:? "format")
            Prelude.<*> (x Data..:? "lastReferencedTime")
            Prelude.<*> (x Data..:? "lastUpdatedTime")
            Prelude.<*> (x Data..: "dataSetName")
      )

instance Prelude.Hashable DataSetSummary where
  hashWithSalt _salt DataSetSummary' {..} =
    _salt
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` dataSetOrg
      `Prelude.hashWithSalt` format
      `Prelude.hashWithSalt` lastReferencedTime
      `Prelude.hashWithSalt` lastUpdatedTime
      `Prelude.hashWithSalt` dataSetName

instance Prelude.NFData DataSetSummary where
  rnf DataSetSummary' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf dataSetOrg
      `Prelude.seq` Prelude.rnf format
      `Prelude.seq` Prelude.rnf lastReferencedTime
      `Prelude.seq` Prelude.rnf lastUpdatedTime
      `Prelude.seq` Prelude.rnf dataSetName

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
-- Module      : Amazonka.QuickSight.Types.DataSourceSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.DataSourceSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DataSourceType

-- | A @DataSourceSummary@ object that returns a summary of a data source.
--
-- /See:/ 'newDataSourceSummary' smart constructor.
data DataSourceSummary = DataSourceSummary'
  { -- | The arn of the datasource.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the data source was created. This value is
    -- expressed in MM-DD-YYYY HH:MM:SS format.
    createdTime :: Prelude.Maybe Data.POSIX,
    -- | The unique ID of the data source.
    dataSourceId :: Prelude.Maybe Prelude.Text,
    -- | The date and time the data source was last updated. This value is
    -- expressed in MM-DD-YYYY HH:MM:SS format.
    lastUpdatedTime :: Prelude.Maybe Data.POSIX,
    -- | The name of the data source.
    name :: Prelude.Maybe Prelude.Text,
    -- | The type of the data source.
    type' :: Prelude.Maybe DataSourceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataSourceSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'dataSourceSummary_arn' - The arn of the datasource.
--
-- 'createdTime', 'dataSourceSummary_createdTime' - The date and time that the data source was created. This value is
-- expressed in MM-DD-YYYY HH:MM:SS format.
--
-- 'dataSourceId', 'dataSourceSummary_dataSourceId' - The unique ID of the data source.
--
-- 'lastUpdatedTime', 'dataSourceSummary_lastUpdatedTime' - The date and time the data source was last updated. This value is
-- expressed in MM-DD-YYYY HH:MM:SS format.
--
-- 'name', 'dataSourceSummary_name' - The name of the data source.
--
-- 'type'', 'dataSourceSummary_type' - The type of the data source.
newDataSourceSummary ::
  DataSourceSummary
newDataSourceSummary =
  DataSourceSummary'
    { arn = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      dataSourceId = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      name = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The arn of the datasource.
dataSourceSummary_arn :: Lens.Lens' DataSourceSummary (Prelude.Maybe Prelude.Text)
dataSourceSummary_arn = Lens.lens (\DataSourceSummary' {arn} -> arn) (\s@DataSourceSummary' {} a -> s {arn = a} :: DataSourceSummary)

-- | The date and time that the data source was created. This value is
-- expressed in MM-DD-YYYY HH:MM:SS format.
dataSourceSummary_createdTime :: Lens.Lens' DataSourceSummary (Prelude.Maybe Prelude.UTCTime)
dataSourceSummary_createdTime = Lens.lens (\DataSourceSummary' {createdTime} -> createdTime) (\s@DataSourceSummary' {} a -> s {createdTime = a} :: DataSourceSummary) Prelude.. Lens.mapping Data._Time

-- | The unique ID of the data source.
dataSourceSummary_dataSourceId :: Lens.Lens' DataSourceSummary (Prelude.Maybe Prelude.Text)
dataSourceSummary_dataSourceId = Lens.lens (\DataSourceSummary' {dataSourceId} -> dataSourceId) (\s@DataSourceSummary' {} a -> s {dataSourceId = a} :: DataSourceSummary)

-- | The date and time the data source was last updated. This value is
-- expressed in MM-DD-YYYY HH:MM:SS format.
dataSourceSummary_lastUpdatedTime :: Lens.Lens' DataSourceSummary (Prelude.Maybe Prelude.UTCTime)
dataSourceSummary_lastUpdatedTime = Lens.lens (\DataSourceSummary' {lastUpdatedTime} -> lastUpdatedTime) (\s@DataSourceSummary' {} a -> s {lastUpdatedTime = a} :: DataSourceSummary) Prelude.. Lens.mapping Data._Time

-- | The name of the data source.
dataSourceSummary_name :: Lens.Lens' DataSourceSummary (Prelude.Maybe Prelude.Text)
dataSourceSummary_name = Lens.lens (\DataSourceSummary' {name} -> name) (\s@DataSourceSummary' {} a -> s {name = a} :: DataSourceSummary)

-- | The type of the data source.
dataSourceSummary_type :: Lens.Lens' DataSourceSummary (Prelude.Maybe DataSourceType)
dataSourceSummary_type = Lens.lens (\DataSourceSummary' {type'} -> type') (\s@DataSourceSummary' {} a -> s {type' = a} :: DataSourceSummary)

instance Data.FromJSON DataSourceSummary where
  parseJSON =
    Data.withObject
      "DataSourceSummary"
      ( \x ->
          DataSourceSummary'
            Prelude.<$> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "CreatedTime")
            Prelude.<*> (x Data..:? "DataSourceId")
            Prelude.<*> (x Data..:? "LastUpdatedTime")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Type")
      )

instance Prelude.Hashable DataSourceSummary where
  hashWithSalt _salt DataSourceSummary' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` dataSourceId
      `Prelude.hashWithSalt` lastUpdatedTime
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'

instance Prelude.NFData DataSourceSummary where
  rnf DataSourceSummary' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf dataSourceId
      `Prelude.seq` Prelude.rnf lastUpdatedTime
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf type'

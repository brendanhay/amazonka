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
-- Module      : Amazonka.QuickSight.Types.DataAggregation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.DataAggregation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.TopicTimeGranularity

-- | A structure that represents a data aggregation.
--
-- /See:/ 'newDataAggregation' smart constructor.
data DataAggregation = DataAggregation'
  { -- | The level of time precision that is used to aggregate @DateTime@ values.
    datasetRowDateGranularity :: Prelude.Maybe TopicTimeGranularity,
    -- | The column name for the default date.
    defaultDateColumnName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataAggregation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'datasetRowDateGranularity', 'dataAggregation_datasetRowDateGranularity' - The level of time precision that is used to aggregate @DateTime@ values.
--
-- 'defaultDateColumnName', 'dataAggregation_defaultDateColumnName' - The column name for the default date.
newDataAggregation ::
  DataAggregation
newDataAggregation =
  DataAggregation'
    { datasetRowDateGranularity =
        Prelude.Nothing,
      defaultDateColumnName = Prelude.Nothing
    }

-- | The level of time precision that is used to aggregate @DateTime@ values.
dataAggregation_datasetRowDateGranularity :: Lens.Lens' DataAggregation (Prelude.Maybe TopicTimeGranularity)
dataAggregation_datasetRowDateGranularity = Lens.lens (\DataAggregation' {datasetRowDateGranularity} -> datasetRowDateGranularity) (\s@DataAggregation' {} a -> s {datasetRowDateGranularity = a} :: DataAggregation)

-- | The column name for the default date.
dataAggregation_defaultDateColumnName :: Lens.Lens' DataAggregation (Prelude.Maybe Prelude.Text)
dataAggregation_defaultDateColumnName = Lens.lens (\DataAggregation' {defaultDateColumnName} -> defaultDateColumnName) (\s@DataAggregation' {} a -> s {defaultDateColumnName = a} :: DataAggregation)

instance Data.FromJSON DataAggregation where
  parseJSON =
    Data.withObject
      "DataAggregation"
      ( \x ->
          DataAggregation'
            Prelude.<$> (x Data..:? "DatasetRowDateGranularity")
            Prelude.<*> (x Data..:? "DefaultDateColumnName")
      )

instance Prelude.Hashable DataAggregation where
  hashWithSalt _salt DataAggregation' {..} =
    _salt
      `Prelude.hashWithSalt` datasetRowDateGranularity
      `Prelude.hashWithSalt` defaultDateColumnName

instance Prelude.NFData DataAggregation where
  rnf DataAggregation' {..} =
    Prelude.rnf datasetRowDateGranularity
      `Prelude.seq` Prelude.rnf defaultDateColumnName

instance Data.ToJSON DataAggregation where
  toJSON DataAggregation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DatasetRowDateGranularity" Data..=)
              Prelude.<$> datasetRowDateGranularity,
            ("DefaultDateColumnName" Data..=)
              Prelude.<$> defaultDateColumnName
          ]
      )

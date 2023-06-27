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
-- Module      : Amazonka.QuickSight.Types.DatasetMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.DatasetMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DataAggregation
import Amazonka.QuickSight.Types.TopicCalculatedField
import Amazonka.QuickSight.Types.TopicColumn
import Amazonka.QuickSight.Types.TopicFilter
import Amazonka.QuickSight.Types.TopicNamedEntity

-- | A structure that represents a dataset.
--
-- /See:/ 'newDatasetMetadata' smart constructor.
data DatasetMetadata = DatasetMetadata'
  { -- | The list of calculated field definitions.
    calculatedFields :: Prelude.Maybe [TopicCalculatedField],
    -- | The list of column definitions.
    columns :: Prelude.Maybe [TopicColumn],
    -- | The definition of a data aggregation.
    dataAggregation :: Prelude.Maybe DataAggregation,
    -- | The description of the dataset.
    datasetDescription :: Prelude.Maybe Prelude.Text,
    -- | The name of the dataset.
    datasetName :: Prelude.Maybe Prelude.Text,
    -- | The list of filter definitions.
    filters :: Prelude.Maybe [TopicFilter],
    -- | The list of named entities definitions.
    namedEntities :: Prelude.Maybe [TopicNamedEntity],
    -- | The Amazon Resource Name (ARN) of the dataset.
    datasetArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DatasetMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'calculatedFields', 'datasetMetadata_calculatedFields' - The list of calculated field definitions.
--
-- 'columns', 'datasetMetadata_columns' - The list of column definitions.
--
-- 'dataAggregation', 'datasetMetadata_dataAggregation' - The definition of a data aggregation.
--
-- 'datasetDescription', 'datasetMetadata_datasetDescription' - The description of the dataset.
--
-- 'datasetName', 'datasetMetadata_datasetName' - The name of the dataset.
--
-- 'filters', 'datasetMetadata_filters' - The list of filter definitions.
--
-- 'namedEntities', 'datasetMetadata_namedEntities' - The list of named entities definitions.
--
-- 'datasetArn', 'datasetMetadata_datasetArn' - The Amazon Resource Name (ARN) of the dataset.
newDatasetMetadata ::
  -- | 'datasetArn'
  Prelude.Text ->
  DatasetMetadata
newDatasetMetadata pDatasetArn_ =
  DatasetMetadata'
    { calculatedFields =
        Prelude.Nothing,
      columns = Prelude.Nothing,
      dataAggregation = Prelude.Nothing,
      datasetDescription = Prelude.Nothing,
      datasetName = Prelude.Nothing,
      filters = Prelude.Nothing,
      namedEntities = Prelude.Nothing,
      datasetArn = pDatasetArn_
    }

-- | The list of calculated field definitions.
datasetMetadata_calculatedFields :: Lens.Lens' DatasetMetadata (Prelude.Maybe [TopicCalculatedField])
datasetMetadata_calculatedFields = Lens.lens (\DatasetMetadata' {calculatedFields} -> calculatedFields) (\s@DatasetMetadata' {} a -> s {calculatedFields = a} :: DatasetMetadata) Prelude.. Lens.mapping Lens.coerced

-- | The list of column definitions.
datasetMetadata_columns :: Lens.Lens' DatasetMetadata (Prelude.Maybe [TopicColumn])
datasetMetadata_columns = Lens.lens (\DatasetMetadata' {columns} -> columns) (\s@DatasetMetadata' {} a -> s {columns = a} :: DatasetMetadata) Prelude.. Lens.mapping Lens.coerced

-- | The definition of a data aggregation.
datasetMetadata_dataAggregation :: Lens.Lens' DatasetMetadata (Prelude.Maybe DataAggregation)
datasetMetadata_dataAggregation = Lens.lens (\DatasetMetadata' {dataAggregation} -> dataAggregation) (\s@DatasetMetadata' {} a -> s {dataAggregation = a} :: DatasetMetadata)

-- | The description of the dataset.
datasetMetadata_datasetDescription :: Lens.Lens' DatasetMetadata (Prelude.Maybe Prelude.Text)
datasetMetadata_datasetDescription = Lens.lens (\DatasetMetadata' {datasetDescription} -> datasetDescription) (\s@DatasetMetadata' {} a -> s {datasetDescription = a} :: DatasetMetadata)

-- | The name of the dataset.
datasetMetadata_datasetName :: Lens.Lens' DatasetMetadata (Prelude.Maybe Prelude.Text)
datasetMetadata_datasetName = Lens.lens (\DatasetMetadata' {datasetName} -> datasetName) (\s@DatasetMetadata' {} a -> s {datasetName = a} :: DatasetMetadata)

-- | The list of filter definitions.
datasetMetadata_filters :: Lens.Lens' DatasetMetadata (Prelude.Maybe [TopicFilter])
datasetMetadata_filters = Lens.lens (\DatasetMetadata' {filters} -> filters) (\s@DatasetMetadata' {} a -> s {filters = a} :: DatasetMetadata) Prelude.. Lens.mapping Lens.coerced

-- | The list of named entities definitions.
datasetMetadata_namedEntities :: Lens.Lens' DatasetMetadata (Prelude.Maybe [TopicNamedEntity])
datasetMetadata_namedEntities = Lens.lens (\DatasetMetadata' {namedEntities} -> namedEntities) (\s@DatasetMetadata' {} a -> s {namedEntities = a} :: DatasetMetadata) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the dataset.
datasetMetadata_datasetArn :: Lens.Lens' DatasetMetadata Prelude.Text
datasetMetadata_datasetArn = Lens.lens (\DatasetMetadata' {datasetArn} -> datasetArn) (\s@DatasetMetadata' {} a -> s {datasetArn = a} :: DatasetMetadata)

instance Data.FromJSON DatasetMetadata where
  parseJSON =
    Data.withObject
      "DatasetMetadata"
      ( \x ->
          DatasetMetadata'
            Prelude.<$> ( x
                            Data..:? "CalculatedFields"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "Columns" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "DataAggregation")
            Prelude.<*> (x Data..:? "DatasetDescription")
            Prelude.<*> (x Data..:? "DatasetName")
            Prelude.<*> (x Data..:? "Filters" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "NamedEntities" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "DatasetArn")
      )

instance Prelude.Hashable DatasetMetadata where
  hashWithSalt _salt DatasetMetadata' {..} =
    _salt
      `Prelude.hashWithSalt` calculatedFields
      `Prelude.hashWithSalt` columns
      `Prelude.hashWithSalt` dataAggregation
      `Prelude.hashWithSalt` datasetDescription
      `Prelude.hashWithSalt` datasetName
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` namedEntities
      `Prelude.hashWithSalt` datasetArn

instance Prelude.NFData DatasetMetadata where
  rnf DatasetMetadata' {..} =
    Prelude.rnf calculatedFields
      `Prelude.seq` Prelude.rnf columns
      `Prelude.seq` Prelude.rnf dataAggregation
      `Prelude.seq` Prelude.rnf datasetDescription
      `Prelude.seq` Prelude.rnf datasetName
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf namedEntities
      `Prelude.seq` Prelude.rnf datasetArn

instance Data.ToJSON DatasetMetadata where
  toJSON DatasetMetadata' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CalculatedFields" Data..=)
              Prelude.<$> calculatedFields,
            ("Columns" Data..=) Prelude.<$> columns,
            ("DataAggregation" Data..=)
              Prelude.<$> dataAggregation,
            ("DatasetDescription" Data..=)
              Prelude.<$> datasetDescription,
            ("DatasetName" Data..=) Prelude.<$> datasetName,
            ("Filters" Data..=) Prelude.<$> filters,
            ("NamedEntities" Data..=) Prelude.<$> namedEntities,
            Prelude.Just ("DatasetArn" Data..= datasetArn)
          ]
      )

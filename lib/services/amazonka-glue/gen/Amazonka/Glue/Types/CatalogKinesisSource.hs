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
-- Module      : Amazonka.Glue.Types.CatalogKinesisSource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.CatalogKinesisSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.KinesisStreamingSourceOptions
import Amazonka.Glue.Types.StreamingDataPreviewOptions
import qualified Amazonka.Prelude as Prelude

-- | Specifies a Kinesis data source in the Glue Data Catalog.
--
-- /See:/ 'newCatalogKinesisSource' smart constructor.
data CatalogKinesisSource = CatalogKinesisSource'
  { -- | The amount of time to spend processing each micro batch.
    windowSize :: Prelude.Maybe Prelude.Natural,
    -- | Additional options for the Kinesis streaming data source.
    streamingOptions :: Prelude.Maybe KinesisStreamingSourceOptions,
    -- | Whether to automatically determine the schema from the incoming data.
    detectSchema :: Prelude.Maybe Prelude.Bool,
    -- | Additional options for data preview.
    dataPreviewOptions :: Prelude.Maybe StreamingDataPreviewOptions,
    -- | The name of the data source.
    name :: Prelude.Text,
    -- | The name of the table in the database to read from.
    table :: Prelude.Text,
    -- | The name of the database to read from.
    database :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CatalogKinesisSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'windowSize', 'catalogKinesisSource_windowSize' - The amount of time to spend processing each micro batch.
--
-- 'streamingOptions', 'catalogKinesisSource_streamingOptions' - Additional options for the Kinesis streaming data source.
--
-- 'detectSchema', 'catalogKinesisSource_detectSchema' - Whether to automatically determine the schema from the incoming data.
--
-- 'dataPreviewOptions', 'catalogKinesisSource_dataPreviewOptions' - Additional options for data preview.
--
-- 'name', 'catalogKinesisSource_name' - The name of the data source.
--
-- 'table', 'catalogKinesisSource_table' - The name of the table in the database to read from.
--
-- 'database', 'catalogKinesisSource_database' - The name of the database to read from.
newCatalogKinesisSource ::
  -- | 'name'
  Prelude.Text ->
  -- | 'table'
  Prelude.Text ->
  -- | 'database'
  Prelude.Text ->
  CatalogKinesisSource
newCatalogKinesisSource pName_ pTable_ pDatabase_ =
  CatalogKinesisSource'
    { windowSize = Prelude.Nothing,
      streamingOptions = Prelude.Nothing,
      detectSchema = Prelude.Nothing,
      dataPreviewOptions = Prelude.Nothing,
      name = pName_,
      table = pTable_,
      database = pDatabase_
    }

-- | The amount of time to spend processing each micro batch.
catalogKinesisSource_windowSize :: Lens.Lens' CatalogKinesisSource (Prelude.Maybe Prelude.Natural)
catalogKinesisSource_windowSize = Lens.lens (\CatalogKinesisSource' {windowSize} -> windowSize) (\s@CatalogKinesisSource' {} a -> s {windowSize = a} :: CatalogKinesisSource)

-- | Additional options for the Kinesis streaming data source.
catalogKinesisSource_streamingOptions :: Lens.Lens' CatalogKinesisSource (Prelude.Maybe KinesisStreamingSourceOptions)
catalogKinesisSource_streamingOptions = Lens.lens (\CatalogKinesisSource' {streamingOptions} -> streamingOptions) (\s@CatalogKinesisSource' {} a -> s {streamingOptions = a} :: CatalogKinesisSource)

-- | Whether to automatically determine the schema from the incoming data.
catalogKinesisSource_detectSchema :: Lens.Lens' CatalogKinesisSource (Prelude.Maybe Prelude.Bool)
catalogKinesisSource_detectSchema = Lens.lens (\CatalogKinesisSource' {detectSchema} -> detectSchema) (\s@CatalogKinesisSource' {} a -> s {detectSchema = a} :: CatalogKinesisSource)

-- | Additional options for data preview.
catalogKinesisSource_dataPreviewOptions :: Lens.Lens' CatalogKinesisSource (Prelude.Maybe StreamingDataPreviewOptions)
catalogKinesisSource_dataPreviewOptions = Lens.lens (\CatalogKinesisSource' {dataPreviewOptions} -> dataPreviewOptions) (\s@CatalogKinesisSource' {} a -> s {dataPreviewOptions = a} :: CatalogKinesisSource)

-- | The name of the data source.
catalogKinesisSource_name :: Lens.Lens' CatalogKinesisSource Prelude.Text
catalogKinesisSource_name = Lens.lens (\CatalogKinesisSource' {name} -> name) (\s@CatalogKinesisSource' {} a -> s {name = a} :: CatalogKinesisSource)

-- | The name of the table in the database to read from.
catalogKinesisSource_table :: Lens.Lens' CatalogKinesisSource Prelude.Text
catalogKinesisSource_table = Lens.lens (\CatalogKinesisSource' {table} -> table) (\s@CatalogKinesisSource' {} a -> s {table = a} :: CatalogKinesisSource)

-- | The name of the database to read from.
catalogKinesisSource_database :: Lens.Lens' CatalogKinesisSource Prelude.Text
catalogKinesisSource_database = Lens.lens (\CatalogKinesisSource' {database} -> database) (\s@CatalogKinesisSource' {} a -> s {database = a} :: CatalogKinesisSource)

instance Data.FromJSON CatalogKinesisSource where
  parseJSON =
    Data.withObject
      "CatalogKinesisSource"
      ( \x ->
          CatalogKinesisSource'
            Prelude.<$> (x Data..:? "WindowSize")
            Prelude.<*> (x Data..:? "StreamingOptions")
            Prelude.<*> (x Data..:? "DetectSchema")
            Prelude.<*> (x Data..:? "DataPreviewOptions")
            Prelude.<*> (x Data..: "Name")
            Prelude.<*> (x Data..: "Table")
            Prelude.<*> (x Data..: "Database")
      )

instance Prelude.Hashable CatalogKinesisSource where
  hashWithSalt _salt CatalogKinesisSource' {..} =
    _salt `Prelude.hashWithSalt` windowSize
      `Prelude.hashWithSalt` streamingOptions
      `Prelude.hashWithSalt` detectSchema
      `Prelude.hashWithSalt` dataPreviewOptions
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` table
      `Prelude.hashWithSalt` database

instance Prelude.NFData CatalogKinesisSource where
  rnf CatalogKinesisSource' {..} =
    Prelude.rnf windowSize
      `Prelude.seq` Prelude.rnf streamingOptions
      `Prelude.seq` Prelude.rnf detectSchema
      `Prelude.seq` Prelude.rnf dataPreviewOptions
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf table
      `Prelude.seq` Prelude.rnf database

instance Data.ToJSON CatalogKinesisSource where
  toJSON CatalogKinesisSource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("WindowSize" Data..=) Prelude.<$> windowSize,
            ("StreamingOptions" Data..=)
              Prelude.<$> streamingOptions,
            ("DetectSchema" Data..=) Prelude.<$> detectSchema,
            ("DataPreviewOptions" Data..=)
              Prelude.<$> dataPreviewOptions,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Table" Data..= table),
            Prelude.Just ("Database" Data..= database)
          ]
      )

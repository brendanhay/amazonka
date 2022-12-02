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
-- Module      : Amazonka.Glue.Types.CatalogKafkaSource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.CatalogKafkaSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.KafkaStreamingSourceOptions
import Amazonka.Glue.Types.StreamingDataPreviewOptions
import qualified Amazonka.Prelude as Prelude

-- | Specifies an Apache Kafka data store in the Data Catalog.
--
-- /See:/ 'newCatalogKafkaSource' smart constructor.
data CatalogKafkaSource = CatalogKafkaSource'
  { -- | The amount of time to spend processing each micro batch.
    windowSize :: Prelude.Maybe Prelude.Natural,
    -- | Specifies the streaming options.
    streamingOptions :: Prelude.Maybe KafkaStreamingSourceOptions,
    -- | Whether to automatically determine the schema from the incoming data.
    detectSchema :: Prelude.Maybe Prelude.Bool,
    -- | Specifies options related to data preview for viewing a sample of your
    -- data.
    dataPreviewOptions :: Prelude.Maybe StreamingDataPreviewOptions,
    -- | The name of the data store.
    name :: Prelude.Text,
    -- | The name of the table in the database to read from.
    table :: Prelude.Text,
    -- | The name of the database to read from.
    database :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CatalogKafkaSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'windowSize', 'catalogKafkaSource_windowSize' - The amount of time to spend processing each micro batch.
--
-- 'streamingOptions', 'catalogKafkaSource_streamingOptions' - Specifies the streaming options.
--
-- 'detectSchema', 'catalogKafkaSource_detectSchema' - Whether to automatically determine the schema from the incoming data.
--
-- 'dataPreviewOptions', 'catalogKafkaSource_dataPreviewOptions' - Specifies options related to data preview for viewing a sample of your
-- data.
--
-- 'name', 'catalogKafkaSource_name' - The name of the data store.
--
-- 'table', 'catalogKafkaSource_table' - The name of the table in the database to read from.
--
-- 'database', 'catalogKafkaSource_database' - The name of the database to read from.
newCatalogKafkaSource ::
  -- | 'name'
  Prelude.Text ->
  -- | 'table'
  Prelude.Text ->
  -- | 'database'
  Prelude.Text ->
  CatalogKafkaSource
newCatalogKafkaSource pName_ pTable_ pDatabase_ =
  CatalogKafkaSource'
    { windowSize = Prelude.Nothing,
      streamingOptions = Prelude.Nothing,
      detectSchema = Prelude.Nothing,
      dataPreviewOptions = Prelude.Nothing,
      name = pName_,
      table = pTable_,
      database = pDatabase_
    }

-- | The amount of time to spend processing each micro batch.
catalogKafkaSource_windowSize :: Lens.Lens' CatalogKafkaSource (Prelude.Maybe Prelude.Natural)
catalogKafkaSource_windowSize = Lens.lens (\CatalogKafkaSource' {windowSize} -> windowSize) (\s@CatalogKafkaSource' {} a -> s {windowSize = a} :: CatalogKafkaSource)

-- | Specifies the streaming options.
catalogKafkaSource_streamingOptions :: Lens.Lens' CatalogKafkaSource (Prelude.Maybe KafkaStreamingSourceOptions)
catalogKafkaSource_streamingOptions = Lens.lens (\CatalogKafkaSource' {streamingOptions} -> streamingOptions) (\s@CatalogKafkaSource' {} a -> s {streamingOptions = a} :: CatalogKafkaSource)

-- | Whether to automatically determine the schema from the incoming data.
catalogKafkaSource_detectSchema :: Lens.Lens' CatalogKafkaSource (Prelude.Maybe Prelude.Bool)
catalogKafkaSource_detectSchema = Lens.lens (\CatalogKafkaSource' {detectSchema} -> detectSchema) (\s@CatalogKafkaSource' {} a -> s {detectSchema = a} :: CatalogKafkaSource)

-- | Specifies options related to data preview for viewing a sample of your
-- data.
catalogKafkaSource_dataPreviewOptions :: Lens.Lens' CatalogKafkaSource (Prelude.Maybe StreamingDataPreviewOptions)
catalogKafkaSource_dataPreviewOptions = Lens.lens (\CatalogKafkaSource' {dataPreviewOptions} -> dataPreviewOptions) (\s@CatalogKafkaSource' {} a -> s {dataPreviewOptions = a} :: CatalogKafkaSource)

-- | The name of the data store.
catalogKafkaSource_name :: Lens.Lens' CatalogKafkaSource Prelude.Text
catalogKafkaSource_name = Lens.lens (\CatalogKafkaSource' {name} -> name) (\s@CatalogKafkaSource' {} a -> s {name = a} :: CatalogKafkaSource)

-- | The name of the table in the database to read from.
catalogKafkaSource_table :: Lens.Lens' CatalogKafkaSource Prelude.Text
catalogKafkaSource_table = Lens.lens (\CatalogKafkaSource' {table} -> table) (\s@CatalogKafkaSource' {} a -> s {table = a} :: CatalogKafkaSource)

-- | The name of the database to read from.
catalogKafkaSource_database :: Lens.Lens' CatalogKafkaSource Prelude.Text
catalogKafkaSource_database = Lens.lens (\CatalogKafkaSource' {database} -> database) (\s@CatalogKafkaSource' {} a -> s {database = a} :: CatalogKafkaSource)

instance Data.FromJSON CatalogKafkaSource where
  parseJSON =
    Data.withObject
      "CatalogKafkaSource"
      ( \x ->
          CatalogKafkaSource'
            Prelude.<$> (x Data..:? "WindowSize")
            Prelude.<*> (x Data..:? "StreamingOptions")
            Prelude.<*> (x Data..:? "DetectSchema")
            Prelude.<*> (x Data..:? "DataPreviewOptions")
            Prelude.<*> (x Data..: "Name")
            Prelude.<*> (x Data..: "Table")
            Prelude.<*> (x Data..: "Database")
      )

instance Prelude.Hashable CatalogKafkaSource where
  hashWithSalt _salt CatalogKafkaSource' {..} =
    _salt `Prelude.hashWithSalt` windowSize
      `Prelude.hashWithSalt` streamingOptions
      `Prelude.hashWithSalt` detectSchema
      `Prelude.hashWithSalt` dataPreviewOptions
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` table
      `Prelude.hashWithSalt` database

instance Prelude.NFData CatalogKafkaSource where
  rnf CatalogKafkaSource' {..} =
    Prelude.rnf windowSize
      `Prelude.seq` Prelude.rnf streamingOptions
      `Prelude.seq` Prelude.rnf detectSchema
      `Prelude.seq` Prelude.rnf dataPreviewOptions
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf table
      `Prelude.seq` Prelude.rnf database

instance Data.ToJSON CatalogKafkaSource where
  toJSON CatalogKafkaSource' {..} =
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

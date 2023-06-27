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
-- Module      : Amazonka.FinSpaceData.Types.DataViewDestinationTypeParams
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FinSpaceData.Types.DataViewDestinationTypeParams where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FinSpaceData.Types.ExportFileFormat
import qualified Amazonka.Prelude as Prelude

-- | Structure for the Dataview destination type parameters.
--
-- /See:/ 'newDataViewDestinationTypeParams' smart constructor.
data DataViewDestinationTypeParams = DataViewDestinationTypeParams'
  { -- | Dataview export file format.
    --
    -- -   @PARQUET@ – Parquet export file format.
    --
    -- -   @DELIMITED_TEXT@ – Delimited text export file format.
    s3DestinationExportFileFormat :: Prelude.Maybe ExportFileFormat,
    -- | Format Options for S3 Destination type.
    --
    -- Here is an example of how you could specify the
    -- @s3DestinationExportFileFormatOptions@
    --
    -- @ { \"header\": \"true\", \"delimiter\": \",\", \"compression\": \"gzip\" }@
    s3DestinationExportFileFormatOptions :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Destination type for a Dataview.
    --
    -- -   @GLUE_TABLE@ – Glue table destination type.
    --
    -- -   @S3@ – S3 destination type.
    destinationType :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataViewDestinationTypeParams' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3DestinationExportFileFormat', 'dataViewDestinationTypeParams_s3DestinationExportFileFormat' - Dataview export file format.
--
-- -   @PARQUET@ – Parquet export file format.
--
-- -   @DELIMITED_TEXT@ – Delimited text export file format.
--
-- 's3DestinationExportFileFormatOptions', 'dataViewDestinationTypeParams_s3DestinationExportFileFormatOptions' - Format Options for S3 Destination type.
--
-- Here is an example of how you could specify the
-- @s3DestinationExportFileFormatOptions@
--
-- @ { \"header\": \"true\", \"delimiter\": \",\", \"compression\": \"gzip\" }@
--
-- 'destinationType', 'dataViewDestinationTypeParams_destinationType' - Destination type for a Dataview.
--
-- -   @GLUE_TABLE@ – Glue table destination type.
--
-- -   @S3@ – S3 destination type.
newDataViewDestinationTypeParams ::
  -- | 'destinationType'
  Prelude.Text ->
  DataViewDestinationTypeParams
newDataViewDestinationTypeParams pDestinationType_ =
  DataViewDestinationTypeParams'
    { s3DestinationExportFileFormat =
        Prelude.Nothing,
      s3DestinationExportFileFormatOptions =
        Prelude.Nothing,
      destinationType = pDestinationType_
    }

-- | Dataview export file format.
--
-- -   @PARQUET@ – Parquet export file format.
--
-- -   @DELIMITED_TEXT@ – Delimited text export file format.
dataViewDestinationTypeParams_s3DestinationExportFileFormat :: Lens.Lens' DataViewDestinationTypeParams (Prelude.Maybe ExportFileFormat)
dataViewDestinationTypeParams_s3DestinationExportFileFormat = Lens.lens (\DataViewDestinationTypeParams' {s3DestinationExportFileFormat} -> s3DestinationExportFileFormat) (\s@DataViewDestinationTypeParams' {} a -> s {s3DestinationExportFileFormat = a} :: DataViewDestinationTypeParams)

-- | Format Options for S3 Destination type.
--
-- Here is an example of how you could specify the
-- @s3DestinationExportFileFormatOptions@
--
-- @ { \"header\": \"true\", \"delimiter\": \",\", \"compression\": \"gzip\" }@
dataViewDestinationTypeParams_s3DestinationExportFileFormatOptions :: Lens.Lens' DataViewDestinationTypeParams (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
dataViewDestinationTypeParams_s3DestinationExportFileFormatOptions = Lens.lens (\DataViewDestinationTypeParams' {s3DestinationExportFileFormatOptions} -> s3DestinationExportFileFormatOptions) (\s@DataViewDestinationTypeParams' {} a -> s {s3DestinationExportFileFormatOptions = a} :: DataViewDestinationTypeParams) Prelude.. Lens.mapping Lens.coerced

-- | Destination type for a Dataview.
--
-- -   @GLUE_TABLE@ – Glue table destination type.
--
-- -   @S3@ – S3 destination type.
dataViewDestinationTypeParams_destinationType :: Lens.Lens' DataViewDestinationTypeParams Prelude.Text
dataViewDestinationTypeParams_destinationType = Lens.lens (\DataViewDestinationTypeParams' {destinationType} -> destinationType) (\s@DataViewDestinationTypeParams' {} a -> s {destinationType = a} :: DataViewDestinationTypeParams)

instance Data.FromJSON DataViewDestinationTypeParams where
  parseJSON =
    Data.withObject
      "DataViewDestinationTypeParams"
      ( \x ->
          DataViewDestinationTypeParams'
            Prelude.<$> (x Data..:? "s3DestinationExportFileFormat")
            Prelude.<*> ( x
                            Data..:? "s3DestinationExportFileFormatOptions"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..: "destinationType")
      )

instance
  Prelude.Hashable
    DataViewDestinationTypeParams
  where
  hashWithSalt _salt DataViewDestinationTypeParams' {..} =
    _salt
      `Prelude.hashWithSalt` s3DestinationExportFileFormat
      `Prelude.hashWithSalt` s3DestinationExportFileFormatOptions
      `Prelude.hashWithSalt` destinationType

instance Prelude.NFData DataViewDestinationTypeParams where
  rnf DataViewDestinationTypeParams' {..} =
    Prelude.rnf s3DestinationExportFileFormat
      `Prelude.seq` Prelude.rnf s3DestinationExportFileFormatOptions
      `Prelude.seq` Prelude.rnf destinationType

instance Data.ToJSON DataViewDestinationTypeParams where
  toJSON DataViewDestinationTypeParams' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("s3DestinationExportFileFormat" Data..=)
              Prelude.<$> s3DestinationExportFileFormat,
            ("s3DestinationExportFileFormatOptions" Data..=)
              Prelude.<$> s3DestinationExportFileFormatOptions,
            Prelude.Just
              ("destinationType" Data..= destinationType)
          ]
      )

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
-- Module      : Amazonka.SESV2.Types.ImportDataSource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SESV2.Types.ImportDataSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SESV2.Types.DataFormat

-- | An object that contains details about the data source of the import job.
--
-- /See:/ 'newImportDataSource' smart constructor.
data ImportDataSource = ImportDataSource'
  { -- | An Amazon S3 URL in the format s3:\/\//\<bucket_name>/\//\<object>/.
    s3Url :: Prelude.Text,
    -- | The data format of the import job\'s data source.
    dataFormat :: DataFormat
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportDataSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Url', 'importDataSource_s3Url' - An Amazon S3 URL in the format s3:\/\//\<bucket_name>/\//\<object>/.
--
-- 'dataFormat', 'importDataSource_dataFormat' - The data format of the import job\'s data source.
newImportDataSource ::
  -- | 's3Url'
  Prelude.Text ->
  -- | 'dataFormat'
  DataFormat ->
  ImportDataSource
newImportDataSource pS3Url_ pDataFormat_ =
  ImportDataSource'
    { s3Url = pS3Url_,
      dataFormat = pDataFormat_
    }

-- | An Amazon S3 URL in the format s3:\/\//\<bucket_name>/\//\<object>/.
importDataSource_s3Url :: Lens.Lens' ImportDataSource Prelude.Text
importDataSource_s3Url = Lens.lens (\ImportDataSource' {s3Url} -> s3Url) (\s@ImportDataSource' {} a -> s {s3Url = a} :: ImportDataSource)

-- | The data format of the import job\'s data source.
importDataSource_dataFormat :: Lens.Lens' ImportDataSource DataFormat
importDataSource_dataFormat = Lens.lens (\ImportDataSource' {dataFormat} -> dataFormat) (\s@ImportDataSource' {} a -> s {dataFormat = a} :: ImportDataSource)

instance Data.FromJSON ImportDataSource where
  parseJSON =
    Data.withObject
      "ImportDataSource"
      ( \x ->
          ImportDataSource'
            Prelude.<$> (x Data..: "S3Url")
            Prelude.<*> (x Data..: "DataFormat")
      )

instance Prelude.Hashable ImportDataSource where
  hashWithSalt _salt ImportDataSource' {..} =
    _salt
      `Prelude.hashWithSalt` s3Url
      `Prelude.hashWithSalt` dataFormat

instance Prelude.NFData ImportDataSource where
  rnf ImportDataSource' {..} =
    Prelude.rnf s3Url
      `Prelude.seq` Prelude.rnf dataFormat

instance Data.ToJSON ImportDataSource where
  toJSON ImportDataSource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("S3Url" Data..= s3Url),
            Prelude.Just ("DataFormat" Data..= dataFormat)
          ]
      )

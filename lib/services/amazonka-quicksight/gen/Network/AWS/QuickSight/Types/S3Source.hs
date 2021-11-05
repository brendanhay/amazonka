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
-- Module      : Network.AWS.QuickSight.Types.S3Source
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.QuickSight.Types.S3Source where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.QuickSight.Types.InputColumn
import Network.AWS.QuickSight.Types.UploadSettings

-- | A physical table type for an S3 data source.
--
-- /See:/ 'newS3Source' smart constructor.
data S3Source = S3Source'
  { -- | Information about the format for the S3 source file or files.
    uploadSettings :: Prelude.Maybe UploadSettings,
    -- | The Amazon Resource Name (ARN) for the data source.
    dataSourceArn :: Prelude.Text,
    -- | A physical table type for an S3 data source.
    --
    -- For files that aren\'t JSON, only @STRING@ data types are supported in
    -- input columns.
    inputColumns :: Prelude.NonEmpty InputColumn
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3Source' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'uploadSettings', 's3Source_uploadSettings' - Information about the format for the S3 source file or files.
--
-- 'dataSourceArn', 's3Source_dataSourceArn' - The Amazon Resource Name (ARN) for the data source.
--
-- 'inputColumns', 's3Source_inputColumns' - A physical table type for an S3 data source.
--
-- For files that aren\'t JSON, only @STRING@ data types are supported in
-- input columns.
newS3Source ::
  -- | 'dataSourceArn'
  Prelude.Text ->
  -- | 'inputColumns'
  Prelude.NonEmpty InputColumn ->
  S3Source
newS3Source pDataSourceArn_ pInputColumns_ =
  S3Source'
    { uploadSettings = Prelude.Nothing,
      dataSourceArn = pDataSourceArn_,
      inputColumns = Lens.coerced Lens.# pInputColumns_
    }

-- | Information about the format for the S3 source file or files.
s3Source_uploadSettings :: Lens.Lens' S3Source (Prelude.Maybe UploadSettings)
s3Source_uploadSettings = Lens.lens (\S3Source' {uploadSettings} -> uploadSettings) (\s@S3Source' {} a -> s {uploadSettings = a} :: S3Source)

-- | The Amazon Resource Name (ARN) for the data source.
s3Source_dataSourceArn :: Lens.Lens' S3Source Prelude.Text
s3Source_dataSourceArn = Lens.lens (\S3Source' {dataSourceArn} -> dataSourceArn) (\s@S3Source' {} a -> s {dataSourceArn = a} :: S3Source)

-- | A physical table type for an S3 data source.
--
-- For files that aren\'t JSON, only @STRING@ data types are supported in
-- input columns.
s3Source_inputColumns :: Lens.Lens' S3Source (Prelude.NonEmpty InputColumn)
s3Source_inputColumns = Lens.lens (\S3Source' {inputColumns} -> inputColumns) (\s@S3Source' {} a -> s {inputColumns = a} :: S3Source) Prelude.. Lens.coerced

instance Core.FromJSON S3Source where
  parseJSON =
    Core.withObject
      "S3Source"
      ( \x ->
          S3Source'
            Prelude.<$> (x Core..:? "UploadSettings")
            Prelude.<*> (x Core..: "DataSourceArn")
            Prelude.<*> (x Core..: "InputColumns")
      )

instance Prelude.Hashable S3Source

instance Prelude.NFData S3Source

instance Core.ToJSON S3Source where
  toJSON S3Source' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("UploadSettings" Core..=)
              Prelude.<$> uploadSettings,
            Prelude.Just ("DataSourceArn" Core..= dataSourceArn),
            Prelude.Just ("InputColumns" Core..= inputColumns)
          ]
      )

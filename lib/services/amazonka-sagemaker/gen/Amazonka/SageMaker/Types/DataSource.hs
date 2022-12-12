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
-- Module      : Amazonka.SageMaker.Types.DataSource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.DataSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.FileSystemDataSource
import Amazonka.SageMaker.Types.S3DataSource

-- | Describes the location of the channel data.
--
-- /See:/ 'newDataSource' smart constructor.
data DataSource = DataSource'
  { -- | The file system that is associated with a channel.
    fileSystemDataSource :: Prelude.Maybe FileSystemDataSource,
    -- | The S3 location of the data source that is associated with a channel.
    s3DataSource :: Prelude.Maybe S3DataSource
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fileSystemDataSource', 'dataSource_fileSystemDataSource' - The file system that is associated with a channel.
--
-- 's3DataSource', 'dataSource_s3DataSource' - The S3 location of the data source that is associated with a channel.
newDataSource ::
  DataSource
newDataSource =
  DataSource'
    { fileSystemDataSource = Prelude.Nothing,
      s3DataSource = Prelude.Nothing
    }

-- | The file system that is associated with a channel.
dataSource_fileSystemDataSource :: Lens.Lens' DataSource (Prelude.Maybe FileSystemDataSource)
dataSource_fileSystemDataSource = Lens.lens (\DataSource' {fileSystemDataSource} -> fileSystemDataSource) (\s@DataSource' {} a -> s {fileSystemDataSource = a} :: DataSource)

-- | The S3 location of the data source that is associated with a channel.
dataSource_s3DataSource :: Lens.Lens' DataSource (Prelude.Maybe S3DataSource)
dataSource_s3DataSource = Lens.lens (\DataSource' {s3DataSource} -> s3DataSource) (\s@DataSource' {} a -> s {s3DataSource = a} :: DataSource)

instance Data.FromJSON DataSource where
  parseJSON =
    Data.withObject
      "DataSource"
      ( \x ->
          DataSource'
            Prelude.<$> (x Data..:? "FileSystemDataSource")
            Prelude.<*> (x Data..:? "S3DataSource")
      )

instance Prelude.Hashable DataSource where
  hashWithSalt _salt DataSource' {..} =
    _salt `Prelude.hashWithSalt` fileSystemDataSource
      `Prelude.hashWithSalt` s3DataSource

instance Prelude.NFData DataSource where
  rnf DataSource' {..} =
    Prelude.rnf fileSystemDataSource
      `Prelude.seq` Prelude.rnf s3DataSource

instance Data.ToJSON DataSource where
  toJSON DataSource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FileSystemDataSource" Data..=)
              Prelude.<$> fileSystemDataSource,
            ("S3DataSource" Data..=) Prelude.<$> s3DataSource
          ]
      )

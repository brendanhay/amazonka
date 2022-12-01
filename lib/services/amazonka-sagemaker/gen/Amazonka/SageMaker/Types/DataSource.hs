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
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.FileSystemDataSource
import Amazonka.SageMaker.Types.S3DataSource

-- | Describes the location of the channel data.
--
-- /See:/ 'newDataSource' smart constructor.
data DataSource = DataSource'
  { -- | The S3 location of the data source that is associated with a channel.
    s3DataSource :: Prelude.Maybe S3DataSource,
    -- | The file system that is associated with a channel.
    fileSystemDataSource :: Prelude.Maybe FileSystemDataSource
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
-- 's3DataSource', 'dataSource_s3DataSource' - The S3 location of the data source that is associated with a channel.
--
-- 'fileSystemDataSource', 'dataSource_fileSystemDataSource' - The file system that is associated with a channel.
newDataSource ::
  DataSource
newDataSource =
  DataSource'
    { s3DataSource = Prelude.Nothing,
      fileSystemDataSource = Prelude.Nothing
    }

-- | The S3 location of the data source that is associated with a channel.
dataSource_s3DataSource :: Lens.Lens' DataSource (Prelude.Maybe S3DataSource)
dataSource_s3DataSource = Lens.lens (\DataSource' {s3DataSource} -> s3DataSource) (\s@DataSource' {} a -> s {s3DataSource = a} :: DataSource)

-- | The file system that is associated with a channel.
dataSource_fileSystemDataSource :: Lens.Lens' DataSource (Prelude.Maybe FileSystemDataSource)
dataSource_fileSystemDataSource = Lens.lens (\DataSource' {fileSystemDataSource} -> fileSystemDataSource) (\s@DataSource' {} a -> s {fileSystemDataSource = a} :: DataSource)

instance Core.FromJSON DataSource where
  parseJSON =
    Core.withObject
      "DataSource"
      ( \x ->
          DataSource'
            Prelude.<$> (x Core..:? "S3DataSource")
            Prelude.<*> (x Core..:? "FileSystemDataSource")
      )

instance Prelude.Hashable DataSource where
  hashWithSalt _salt DataSource' {..} =
    _salt `Prelude.hashWithSalt` s3DataSource
      `Prelude.hashWithSalt` fileSystemDataSource

instance Prelude.NFData DataSource where
  rnf DataSource' {..} =
    Prelude.rnf s3DataSource
      `Prelude.seq` Prelude.rnf fileSystemDataSource

instance Core.ToJSON DataSource where
  toJSON DataSource' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("S3DataSource" Core..=) Prelude.<$> s3DataSource,
            ("FileSystemDataSource" Core..=)
              Prelude.<$> fileSystemDataSource
          ]
      )

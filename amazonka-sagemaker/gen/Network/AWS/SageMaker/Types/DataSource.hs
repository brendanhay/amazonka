{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.SageMaker.Types.DataSource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.DataSource where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.FileSystemDataSource
import Network.AWS.SageMaker.Types.S3DataSource

-- | Describes the location of the channel data.
--
-- /See:/ 'newDataSource' smart constructor.
data DataSource = DataSource'
  { -- | The file system that is associated with a channel.
    fileSystemDataSource :: Prelude.Maybe FileSystemDataSource,
    -- | The S3 location of the data source that is associated with a channel.
    s3DataSource :: Prelude.Maybe S3DataSource
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON DataSource where
  parseJSON =
    Prelude.withObject
      "DataSource"
      ( \x ->
          DataSource'
            Prelude.<$> (x Prelude..:? "FileSystemDataSource")
            Prelude.<*> (x Prelude..:? "S3DataSource")
      )

instance Prelude.Hashable DataSource

instance Prelude.NFData DataSource

instance Prelude.ToJSON DataSource where
  toJSON DataSource' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("FileSystemDataSource" Prelude..=)
              Prelude.<$> fileSystemDataSource,
            ("S3DataSource" Prelude..=)
              Prelude.<$> s3DataSource
          ]
      )

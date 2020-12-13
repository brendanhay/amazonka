{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.DataSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.DataSource
  ( DataSource (..),

    -- * Smart constructor
    mkDataSource,

    -- * Lenses
    dsS3DataSource,
    dsFileSystemDataSource,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.FileSystemDataSource
import Network.AWS.SageMaker.Types.S3DataSource

-- | Describes the location of the channel data.
--
-- /See:/ 'mkDataSource' smart constructor.
data DataSource = DataSource'
  { -- | The S3 location of the data source that is associated with a channel.
    s3DataSource :: Lude.Maybe S3DataSource,
    -- | The file system that is associated with a channel.
    fileSystemDataSource :: Lude.Maybe FileSystemDataSource
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DataSource' with the minimum fields required to make a request.
--
-- * 's3DataSource' - The S3 location of the data source that is associated with a channel.
-- * 'fileSystemDataSource' - The file system that is associated with a channel.
mkDataSource ::
  DataSource
mkDataSource =
  DataSource'
    { s3DataSource = Lude.Nothing,
      fileSystemDataSource = Lude.Nothing
    }

-- | The S3 location of the data source that is associated with a channel.
--
-- /Note:/ Consider using 's3DataSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsS3DataSource :: Lens.Lens' DataSource (Lude.Maybe S3DataSource)
dsS3DataSource = Lens.lens (s3DataSource :: DataSource -> Lude.Maybe S3DataSource) (\s a -> s {s3DataSource = a} :: DataSource)
{-# DEPRECATED dsS3DataSource "Use generic-lens or generic-optics with 's3DataSource' instead." #-}

-- | The file system that is associated with a channel.
--
-- /Note:/ Consider using 'fileSystemDataSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsFileSystemDataSource :: Lens.Lens' DataSource (Lude.Maybe FileSystemDataSource)
dsFileSystemDataSource = Lens.lens (fileSystemDataSource :: DataSource -> Lude.Maybe FileSystemDataSource) (\s a -> s {fileSystemDataSource = a} :: DataSource)
{-# DEPRECATED dsFileSystemDataSource "Use generic-lens or generic-optics with 'fileSystemDataSource' instead." #-}

instance Lude.FromJSON DataSource where
  parseJSON =
    Lude.withObject
      "DataSource"
      ( \x ->
          DataSource'
            Lude.<$> (x Lude..:? "S3DataSource")
            Lude.<*> (x Lude..:? "FileSystemDataSource")
      )

instance Lude.ToJSON DataSource where
  toJSON DataSource' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("S3DataSource" Lude..=) Lude.<$> s3DataSource,
            ("FileSystemDataSource" Lude..=) Lude.<$> fileSystemDataSource
          ]
      )

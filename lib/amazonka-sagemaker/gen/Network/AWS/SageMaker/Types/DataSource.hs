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
    dsFileSystemDataSource,
    dsS3DataSource,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.FileSystemDataSource as Types
import qualified Network.AWS.SageMaker.Types.S3DataSource as Types

-- | Describes the location of the channel data.
--
-- /See:/ 'mkDataSource' smart constructor.
data DataSource = DataSource'
  { -- | The file system that is associated with a channel.
    fileSystemDataSource :: Core.Maybe Types.FileSystemDataSource,
    -- | The S3 location of the data source that is associated with a channel.
    s3DataSource :: Core.Maybe Types.S3DataSource
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DataSource' value with any optional fields omitted.
mkDataSource ::
  DataSource
mkDataSource =
  DataSource'
    { fileSystemDataSource = Core.Nothing,
      s3DataSource = Core.Nothing
    }

-- | The file system that is associated with a channel.
--
-- /Note:/ Consider using 'fileSystemDataSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsFileSystemDataSource :: Lens.Lens' DataSource (Core.Maybe Types.FileSystemDataSource)
dsFileSystemDataSource = Lens.field @"fileSystemDataSource"
{-# DEPRECATED dsFileSystemDataSource "Use generic-lens or generic-optics with 'fileSystemDataSource' instead." #-}

-- | The S3 location of the data source that is associated with a channel.
--
-- /Note:/ Consider using 's3DataSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsS3DataSource :: Lens.Lens' DataSource (Core.Maybe Types.S3DataSource)
dsS3DataSource = Lens.field @"s3DataSource"
{-# DEPRECATED dsS3DataSource "Use generic-lens or generic-optics with 's3DataSource' instead." #-}

instance Core.FromJSON DataSource where
  toJSON DataSource {..} =
    Core.object
      ( Core.catMaybes
          [ ("FileSystemDataSource" Core..=) Core.<$> fileSystemDataSource,
            ("S3DataSource" Core..=) Core.<$> s3DataSource
          ]
      )

instance Core.FromJSON DataSource where
  parseJSON =
    Core.withObject "DataSource" Core.$
      \x ->
        DataSource'
          Core.<$> (x Core..:? "FileSystemDataSource")
          Core.<*> (x Core..:? "S3DataSource")

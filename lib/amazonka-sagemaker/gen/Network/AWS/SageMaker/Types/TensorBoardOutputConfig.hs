{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TensorBoardOutputConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TensorBoardOutputConfig
  ( TensorBoardOutputConfig (..),

    -- * Smart constructor
    mkTensorBoardOutputConfig,

    -- * Lenses
    tbocS3OutputPath,
    tbocLocalPath,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.DirectoryPath as Types
import qualified Network.AWS.SageMaker.Types.S3Uri as Types

-- | Configuration of storage locations for TensorBoard output.
--
-- /See:/ 'mkTensorBoardOutputConfig' smart constructor.
data TensorBoardOutputConfig = TensorBoardOutputConfig'
  { -- | Path to Amazon S3 storage location for TensorBoard output.
    s3OutputPath :: Types.S3Uri,
    -- | Path to local storage location for tensorBoard output. Defaults to @/opt/ml/output/tensorboard@ .
    localPath :: Core.Maybe Types.DirectoryPath
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TensorBoardOutputConfig' value with any optional fields omitted.
mkTensorBoardOutputConfig ::
  -- | 's3OutputPath'
  Types.S3Uri ->
  TensorBoardOutputConfig
mkTensorBoardOutputConfig s3OutputPath =
  TensorBoardOutputConfig' {s3OutputPath, localPath = Core.Nothing}

-- | Path to Amazon S3 storage location for TensorBoard output.
--
-- /Note:/ Consider using 's3OutputPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tbocS3OutputPath :: Lens.Lens' TensorBoardOutputConfig Types.S3Uri
tbocS3OutputPath = Lens.field @"s3OutputPath"
{-# DEPRECATED tbocS3OutputPath "Use generic-lens or generic-optics with 's3OutputPath' instead." #-}

-- | Path to local storage location for tensorBoard output. Defaults to @/opt/ml/output/tensorboard@ .
--
-- /Note:/ Consider using 'localPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tbocLocalPath :: Lens.Lens' TensorBoardOutputConfig (Core.Maybe Types.DirectoryPath)
tbocLocalPath = Lens.field @"localPath"
{-# DEPRECATED tbocLocalPath "Use generic-lens or generic-optics with 'localPath' instead." #-}

instance Core.FromJSON TensorBoardOutputConfig where
  toJSON TensorBoardOutputConfig {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("S3OutputPath" Core..= s3OutputPath),
            ("LocalPath" Core..=) Core.<$> localPath
          ]
      )

instance Core.FromJSON TensorBoardOutputConfig where
  parseJSON =
    Core.withObject "TensorBoardOutputConfig" Core.$
      \x ->
        TensorBoardOutputConfig'
          Core.<$> (x Core..: "S3OutputPath") Core.<*> (x Core..:? "LocalPath")

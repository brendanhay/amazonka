{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TransformDataSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TransformDataSource
  ( TransformDataSource (..),

    -- * Smart constructor
    mkTransformDataSource,

    -- * Lenses
    tdsS3DataSource,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.TransformS3DataSource as Types

-- | Describes the location of the channel data.
--
-- /See:/ 'mkTransformDataSource' smart constructor.
newtype TransformDataSource = TransformDataSource'
  { -- | The S3 location of the data source that is associated with a channel.
    s3DataSource :: Types.TransformS3DataSource
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'TransformDataSource' value with any optional fields omitted.
mkTransformDataSource ::
  -- | 's3DataSource'
  Types.TransformS3DataSource ->
  TransformDataSource
mkTransformDataSource s3DataSource =
  TransformDataSource' {s3DataSource}

-- | The S3 location of the data source that is associated with a channel.
--
-- /Note:/ Consider using 's3DataSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdsS3DataSource :: Lens.Lens' TransformDataSource Types.TransformS3DataSource
tdsS3DataSource = Lens.field @"s3DataSource"
{-# DEPRECATED tdsS3DataSource "Use generic-lens or generic-optics with 's3DataSource' instead." #-}

instance Core.FromJSON TransformDataSource where
  toJSON TransformDataSource {..} =
    Core.object
      (Core.catMaybes [Core.Just ("S3DataSource" Core..= s3DataSource)])

instance Core.FromJSON TransformDataSource where
  parseJSON =
    Core.withObject "TransformDataSource" Core.$
      \x -> TransformDataSource' Core.<$> (x Core..: "S3DataSource")

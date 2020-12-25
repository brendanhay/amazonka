{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AutoMLDataSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AutoMLDataSource
  ( AutoMLDataSource (..),

    -- * Smart constructor
    mkAutoMLDataSource,

    -- * Lenses
    amldsS3DataSource,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.AutoMLS3DataSource as Types

-- | The data source for the Autopilot job.
--
-- /See:/ 'mkAutoMLDataSource' smart constructor.
newtype AutoMLDataSource = AutoMLDataSource'
  { -- | The Amazon S3 location of the input data.
    s3DataSource :: Types.AutoMLS3DataSource
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AutoMLDataSource' value with any optional fields omitted.
mkAutoMLDataSource ::
  -- | 's3DataSource'
  Types.AutoMLS3DataSource ->
  AutoMLDataSource
mkAutoMLDataSource s3DataSource = AutoMLDataSource' {s3DataSource}

-- | The Amazon S3 location of the input data.
--
-- /Note:/ Consider using 's3DataSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amldsS3DataSource :: Lens.Lens' AutoMLDataSource Types.AutoMLS3DataSource
amldsS3DataSource = Lens.field @"s3DataSource"
{-# DEPRECATED amldsS3DataSource "Use generic-lens or generic-optics with 's3DataSource' instead." #-}

instance Core.FromJSON AutoMLDataSource where
  toJSON AutoMLDataSource {..} =
    Core.object
      (Core.catMaybes [Core.Just ("S3DataSource" Core..= s3DataSource)])

instance Core.FromJSON AutoMLDataSource where
  parseJSON =
    Core.withObject "AutoMLDataSource" Core.$
      \x -> AutoMLDataSource' Core.<$> (x Core..: "S3DataSource")

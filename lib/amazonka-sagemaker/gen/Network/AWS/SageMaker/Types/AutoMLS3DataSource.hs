{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AutoMLS3DataSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.AutoMLS3DataSource
  ( AutoMLS3DataSource (..)
  -- * Smart constructor
  , mkAutoMLS3DataSource
  -- * Lenses
  , amlsdsS3DataType
  , amlsdsS3Uri
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.AutoMLS3DataType as Types
import qualified Network.AWS.SageMaker.Types.S3Uri as Types

-- | The Amazon S3 data source.
--
-- /See:/ 'mkAutoMLS3DataSource' smart constructor.
data AutoMLS3DataSource = AutoMLS3DataSource'
  { s3DataType :: Types.AutoMLS3DataType
    -- ^ The data type.
  , s3Uri :: Types.S3Uri
    -- ^ The URL to the Amazon S3 data source.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AutoMLS3DataSource' value with any optional fields omitted.
mkAutoMLS3DataSource
    :: Types.AutoMLS3DataType -- ^ 's3DataType'
    -> Types.S3Uri -- ^ 's3Uri'
    -> AutoMLS3DataSource
mkAutoMLS3DataSource s3DataType s3Uri
  = AutoMLS3DataSource'{s3DataType, s3Uri}

-- | The data type.
--
-- /Note:/ Consider using 's3DataType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amlsdsS3DataType :: Lens.Lens' AutoMLS3DataSource Types.AutoMLS3DataType
amlsdsS3DataType = Lens.field @"s3DataType"
{-# INLINEABLE amlsdsS3DataType #-}
{-# DEPRECATED s3DataType "Use generic-lens or generic-optics with 's3DataType' instead"  #-}

-- | The URL to the Amazon S3 data source.
--
-- /Note:/ Consider using 's3Uri' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amlsdsS3Uri :: Lens.Lens' AutoMLS3DataSource Types.S3Uri
amlsdsS3Uri = Lens.field @"s3Uri"
{-# INLINEABLE amlsdsS3Uri #-}
{-# DEPRECATED s3Uri "Use generic-lens or generic-optics with 's3Uri' instead"  #-}

instance Core.FromJSON AutoMLS3DataSource where
        toJSON AutoMLS3DataSource{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("S3DataType" Core..= s3DataType),
                  Core.Just ("S3Uri" Core..= s3Uri)])

instance Core.FromJSON AutoMLS3DataSource where
        parseJSON
          = Core.withObject "AutoMLS3DataSource" Core.$
              \ x ->
                AutoMLS3DataSource' Core.<$>
                  (x Core..: "S3DataType") Core.<*> x Core..: "S3Uri"

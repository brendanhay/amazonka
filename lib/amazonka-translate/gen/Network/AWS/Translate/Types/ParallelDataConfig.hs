{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Translate.Types.ParallelDataConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Translate.Types.ParallelDataConfig
  ( ParallelDataConfig (..)
  -- * Smart constructor
  , mkParallelDataConfig
  -- * Lenses
  , pdcS3Uri
  , pdcFormat
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Translate.Types.ParallelDataFormat as Types
import qualified Network.AWS.Translate.Types.S3Uri as Types

-- | Specifies the format and S3 location of the parallel data input file.
--
-- /See:/ 'mkParallelDataConfig' smart constructor.
data ParallelDataConfig = ParallelDataConfig'
  { s3Uri :: Types.S3Uri
    -- ^ The URI of the Amazon S3 folder that contains the parallel data input file. The folder must be in the same Region as the API endpoint you are calling.
  , format :: Types.ParallelDataFormat
    -- ^ The format of the parallel data input file.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ParallelDataConfig' value with any optional fields omitted.
mkParallelDataConfig
    :: Types.S3Uri -- ^ 's3Uri'
    -> Types.ParallelDataFormat -- ^ 'format'
    -> ParallelDataConfig
mkParallelDataConfig s3Uri format
  = ParallelDataConfig'{s3Uri, format}

-- | The URI of the Amazon S3 folder that contains the parallel data input file. The folder must be in the same Region as the API endpoint you are calling.
--
-- /Note:/ Consider using 's3Uri' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdcS3Uri :: Lens.Lens' ParallelDataConfig Types.S3Uri
pdcS3Uri = Lens.field @"s3Uri"
{-# INLINEABLE pdcS3Uri #-}
{-# DEPRECATED s3Uri "Use generic-lens or generic-optics with 's3Uri' instead"  #-}

-- | The format of the parallel data input file.
--
-- /Note:/ Consider using 'format' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdcFormat :: Lens.Lens' ParallelDataConfig Types.ParallelDataFormat
pdcFormat = Lens.field @"format"
{-# INLINEABLE pdcFormat #-}
{-# DEPRECATED format "Use generic-lens or generic-optics with 'format' instead"  #-}

instance Core.FromJSON ParallelDataConfig where
        toJSON ParallelDataConfig{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("S3Uri" Core..= s3Uri),
                  Core.Just ("Format" Core..= format)])

instance Core.FromJSON ParallelDataConfig where
        parseJSON
          = Core.withObject "ParallelDataConfig" Core.$
              \ x ->
                ParallelDataConfig' Core.<$>
                  (x Core..: "S3Uri") Core.<*> x Core..: "Format"

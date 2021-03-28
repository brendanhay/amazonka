{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Translate.Types.OutputDataConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Translate.Types.OutputDataConfig
  ( OutputDataConfig (..)
  -- * Smart constructor
  , mkOutputDataConfig
  -- * Lenses
  , odcS3Uri
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Translate.Types.S3Uri as Types

-- | The output configuration properties for a batch translation job.
--
-- /See:/ 'mkOutputDataConfig' smart constructor.
newtype OutputDataConfig = OutputDataConfig'
  { s3Uri :: Types.S3Uri
    -- ^ The URI of the S3 folder that contains a translation job's output file. The folder must be in the same Region as the API endpoint that you are calling.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'OutputDataConfig' value with any optional fields omitted.
mkOutputDataConfig
    :: Types.S3Uri -- ^ 's3Uri'
    -> OutputDataConfig
mkOutputDataConfig s3Uri = OutputDataConfig'{s3Uri}

-- | The URI of the S3 folder that contains a translation job's output file. The folder must be in the same Region as the API endpoint that you are calling.
--
-- /Note:/ Consider using 's3Uri' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odcS3Uri :: Lens.Lens' OutputDataConfig Types.S3Uri
odcS3Uri = Lens.field @"s3Uri"
{-# INLINEABLE odcS3Uri #-}
{-# DEPRECATED s3Uri "Use generic-lens or generic-optics with 's3Uri' instead"  #-}

instance Core.FromJSON OutputDataConfig where
        toJSON OutputDataConfig{..}
          = Core.object (Core.catMaybes [Core.Just ("S3Uri" Core..= s3Uri)])

instance Core.FromJSON OutputDataConfig where
        parseJSON
          = Core.withObject "OutputDataConfig" Core.$
              \ x -> OutputDataConfig' Core.<$> (x Core..: "S3Uri")

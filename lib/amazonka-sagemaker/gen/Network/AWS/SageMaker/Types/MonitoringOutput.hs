{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.MonitoringOutput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.MonitoringOutput
  ( MonitoringOutput (..)
  -- * Smart constructor
  , mkMonitoringOutput
  -- * Lenses
  , moS3Output
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.MonitoringS3Output as Types

-- | The output object for a monitoring job.
--
-- /See:/ 'mkMonitoringOutput' smart constructor.
newtype MonitoringOutput = MonitoringOutput'
  { s3Output :: Types.MonitoringS3Output
    -- ^ The Amazon S3 storage location where the results of a monitoring job are saved.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'MonitoringOutput' value with any optional fields omitted.
mkMonitoringOutput
    :: Types.MonitoringS3Output -- ^ 's3Output'
    -> MonitoringOutput
mkMonitoringOutput s3Output = MonitoringOutput'{s3Output}

-- | The Amazon S3 storage location where the results of a monitoring job are saved.
--
-- /Note:/ Consider using 's3Output' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
moS3Output :: Lens.Lens' MonitoringOutput Types.MonitoringS3Output
moS3Output = Lens.field @"s3Output"
{-# INLINEABLE moS3Output #-}
{-# DEPRECATED s3Output "Use generic-lens or generic-optics with 's3Output' instead"  #-}

instance Core.FromJSON MonitoringOutput where
        toJSON MonitoringOutput{..}
          = Core.object
              (Core.catMaybes [Core.Just ("S3Output" Core..= s3Output)])

instance Core.FromJSON MonitoringOutput where
        parseJSON
          = Core.withObject "MonitoringOutput" Core.$
              \ x -> MonitoringOutput' Core.<$> (x Core..: "S3Output")

{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ProcessingResources
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.ProcessingResources
  ( ProcessingResources (..)
  -- * Smart constructor
  , mkProcessingResources
  -- * Lenses
  , prClusterConfig
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.ProcessingClusterConfig as Types

-- | Identifies the resources, ML compute instances, and ML storage volumes to deploy for a processing job. In distributed training, you specify more than one instance.
--
-- /See:/ 'mkProcessingResources' smart constructor.
newtype ProcessingResources = ProcessingResources'
  { clusterConfig :: Types.ProcessingClusterConfig
    -- ^ The configuration for the resources in a cluster used to run the processing job.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ProcessingResources' value with any optional fields omitted.
mkProcessingResources
    :: Types.ProcessingClusterConfig -- ^ 'clusterConfig'
    -> ProcessingResources
mkProcessingResources clusterConfig
  = ProcessingResources'{clusterConfig}

-- | The configuration for the resources in a cluster used to run the processing job.
--
-- /Note:/ Consider using 'clusterConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prClusterConfig :: Lens.Lens' ProcessingResources Types.ProcessingClusterConfig
prClusterConfig = Lens.field @"clusterConfig"
{-# INLINEABLE prClusterConfig #-}
{-# DEPRECATED clusterConfig "Use generic-lens or generic-optics with 'clusterConfig' instead"  #-}

instance Core.FromJSON ProcessingResources where
        toJSON ProcessingResources{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ClusterConfig" Core..= clusterConfig)])

instance Core.FromJSON ProcessingResources where
        parseJSON
          = Core.withObject "ProcessingResources" Core.$
              \ x -> ProcessingResources' Core.<$> (x Core..: "ClusterConfig")

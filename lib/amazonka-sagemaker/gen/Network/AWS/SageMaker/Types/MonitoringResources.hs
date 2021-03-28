{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.MonitoringResources
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.MonitoringResources
  ( MonitoringResources (..)
  -- * Smart constructor
  , mkMonitoringResources
  -- * Lenses
  , mrClusterConfig
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.MonitoringClusterConfig as Types

-- | Identifies the resources to deploy for a monitoring job.
--
-- /See:/ 'mkMonitoringResources' smart constructor.
newtype MonitoringResources = MonitoringResources'
  { clusterConfig :: Types.MonitoringClusterConfig
    -- ^ The configuration for the cluster resources used to run the processing job.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'MonitoringResources' value with any optional fields omitted.
mkMonitoringResources
    :: Types.MonitoringClusterConfig -- ^ 'clusterConfig'
    -> MonitoringResources
mkMonitoringResources clusterConfig
  = MonitoringResources'{clusterConfig}

-- | The configuration for the cluster resources used to run the processing job.
--
-- /Note:/ Consider using 'clusterConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrClusterConfig :: Lens.Lens' MonitoringResources Types.MonitoringClusterConfig
mrClusterConfig = Lens.field @"clusterConfig"
{-# INLINEABLE mrClusterConfig #-}
{-# DEPRECATED clusterConfig "Use generic-lens or generic-optics with 'clusterConfig' instead"  #-}

instance Core.FromJSON MonitoringResources where
        toJSON MonitoringResources{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ClusterConfig" Core..= clusterConfig)])

instance Core.FromJSON MonitoringResources where
        parseJSON
          = Core.withObject "MonitoringResources" Core.$
              \ x -> MonitoringResources' Core.<$> (x Core..: "ClusterConfig")

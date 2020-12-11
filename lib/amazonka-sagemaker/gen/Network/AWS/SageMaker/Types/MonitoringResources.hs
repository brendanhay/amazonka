-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.MonitoringResources
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.MonitoringResources
  ( MonitoringResources (..),

    -- * Smart constructor
    mkMonitoringResources,

    -- * Lenses
    mrClusterConfig,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.MonitoringClusterConfig

-- | Identifies the resources to deploy for a monitoring job.
--
-- /See:/ 'mkMonitoringResources' smart constructor.
newtype MonitoringResources = MonitoringResources'
  { clusterConfig ::
      MonitoringClusterConfig
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MonitoringResources' with the minimum fields required to make a request.
--
-- * 'clusterConfig' - The configuration for the cluster resources used to run the processing job.
mkMonitoringResources ::
  -- | 'clusterConfig'
  MonitoringClusterConfig ->
  MonitoringResources
mkMonitoringResources pClusterConfig_ =
  MonitoringResources' {clusterConfig = pClusterConfig_}

-- | The configuration for the cluster resources used to run the processing job.
--
-- /Note:/ Consider using 'clusterConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrClusterConfig :: Lens.Lens' MonitoringResources MonitoringClusterConfig
mrClusterConfig = Lens.lens (clusterConfig :: MonitoringResources -> MonitoringClusterConfig) (\s a -> s {clusterConfig = a} :: MonitoringResources)
{-# DEPRECATED mrClusterConfig "Use generic-lens or generic-optics with 'clusterConfig' instead." #-}

instance Lude.FromJSON MonitoringResources where
  parseJSON =
    Lude.withObject
      "MonitoringResources"
      (\x -> MonitoringResources' Lude.<$> (x Lude..: "ClusterConfig"))

instance Lude.ToJSON MonitoringResources where
  toJSON MonitoringResources' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("ClusterConfig" Lude..= clusterConfig)]
      )

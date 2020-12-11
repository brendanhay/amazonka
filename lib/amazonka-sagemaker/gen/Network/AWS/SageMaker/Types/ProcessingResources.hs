-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ProcessingResources
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ProcessingResources
  ( ProcessingResources (..),

    -- * Smart constructor
    mkProcessingResources,

    -- * Lenses
    prClusterConfig,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.ProcessingClusterConfig

-- | Identifies the resources, ML compute instances, and ML storage volumes to deploy for a processing job. In distributed training, you specify more than one instance.
--
-- /See:/ 'mkProcessingResources' smart constructor.
newtype ProcessingResources = ProcessingResources'
  { clusterConfig ::
      ProcessingClusterConfig
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ProcessingResources' with the minimum fields required to make a request.
--
-- * 'clusterConfig' - The configuration for the resources in a cluster used to run the processing job.
mkProcessingResources ::
  -- | 'clusterConfig'
  ProcessingClusterConfig ->
  ProcessingResources
mkProcessingResources pClusterConfig_ =
  ProcessingResources' {clusterConfig = pClusterConfig_}

-- | The configuration for the resources in a cluster used to run the processing job.
--
-- /Note:/ Consider using 'clusterConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prClusterConfig :: Lens.Lens' ProcessingResources ProcessingClusterConfig
prClusterConfig = Lens.lens (clusterConfig :: ProcessingResources -> ProcessingClusterConfig) (\s a -> s {clusterConfig = a} :: ProcessingResources)
{-# DEPRECATED prClusterConfig "Use generic-lens or generic-optics with 'clusterConfig' instead." #-}

instance Lude.FromJSON ProcessingResources where
  parseJSON =
    Lude.withObject
      "ProcessingResources"
      (\x -> ProcessingResources' Lude.<$> (x Lude..: "ClusterConfig"))

instance Lude.ToJSON ProcessingResources where
  toJSON ProcessingResources' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("ClusterConfig" Lude..= clusterConfig)]
      )

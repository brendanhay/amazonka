{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.ResourceConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.ResourceConfiguration
  ( ResourceConfiguration (..),

    -- * Smart constructor
    mkResourceConfiguration,

    -- * Lenses
    rcComputeType,
    rcVolumeSizeInGB,
  )
where

import Network.AWS.IoTAnalytics.Types.ComputeType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The configuration of the resource used to execute the @containerAction@ .
--
-- /See:/ 'mkResourceConfiguration' smart constructor.
data ResourceConfiguration = ResourceConfiguration'
  { computeType ::
      ComputeType,
    volumeSizeInGB :: Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResourceConfiguration' with the minimum fields required to make a request.
--
-- * 'computeType' - The type of the compute resource used to execute the @containerAction@ . Possible values are: @ACU_1@ (vCPU=4, memory=16 GiB) or @ACU_2@ (vCPU=8, memory=32 GiB).
-- * 'volumeSizeInGB' - The size, in GB, of the persistent storage available to the resource instance used to execute the @containerAction@ (min: 1, max: 50).
mkResourceConfiguration ::
  -- | 'computeType'
  ComputeType ->
  -- | 'volumeSizeInGB'
  Lude.Natural ->
  ResourceConfiguration
mkResourceConfiguration pComputeType_ pVolumeSizeInGB_ =
  ResourceConfiguration'
    { computeType = pComputeType_,
      volumeSizeInGB = pVolumeSizeInGB_
    }

-- | The type of the compute resource used to execute the @containerAction@ . Possible values are: @ACU_1@ (vCPU=4, memory=16 GiB) or @ACU_2@ (vCPU=8, memory=32 GiB).
--
-- /Note:/ Consider using 'computeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcComputeType :: Lens.Lens' ResourceConfiguration ComputeType
rcComputeType = Lens.lens (computeType :: ResourceConfiguration -> ComputeType) (\s a -> s {computeType = a} :: ResourceConfiguration)
{-# DEPRECATED rcComputeType "Use generic-lens or generic-optics with 'computeType' instead." #-}

-- | The size, in GB, of the persistent storage available to the resource instance used to execute the @containerAction@ (min: 1, max: 50).
--
-- /Note:/ Consider using 'volumeSizeInGB' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcVolumeSizeInGB :: Lens.Lens' ResourceConfiguration Lude.Natural
rcVolumeSizeInGB = Lens.lens (volumeSizeInGB :: ResourceConfiguration -> Lude.Natural) (\s a -> s {volumeSizeInGB = a} :: ResourceConfiguration)
{-# DEPRECATED rcVolumeSizeInGB "Use generic-lens or generic-optics with 'volumeSizeInGB' instead." #-}

instance Lude.FromJSON ResourceConfiguration where
  parseJSON =
    Lude.withObject
      "ResourceConfiguration"
      ( \x ->
          ResourceConfiguration'
            Lude.<$> (x Lude..: "computeType") Lude.<*> (x Lude..: "volumeSizeInGB")
      )

instance Lude.ToJSON ResourceConfiguration where
  toJSON ResourceConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("computeType" Lude..= computeType),
            Lude.Just ("volumeSizeInGB" Lude..= volumeSizeInGB)
          ]
      )

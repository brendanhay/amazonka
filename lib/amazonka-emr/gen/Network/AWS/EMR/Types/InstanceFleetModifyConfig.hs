{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.InstanceFleetModifyConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.InstanceFleetModifyConfig
  ( InstanceFleetModifyConfig (..),

    -- * Smart constructor
    mkInstanceFleetModifyConfig,

    -- * Lenses
    ifmcTargetOnDemandCapacity,
    ifmcTargetSpotCapacity,
    ifmcInstanceFleetId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Configuration parameters for an instance fleet modification request.
--
-- /See:/ 'mkInstanceFleetModifyConfig' smart constructor.
data InstanceFleetModifyConfig = InstanceFleetModifyConfig'
  { targetOnDemandCapacity ::
      Lude.Maybe Lude.Natural,
    targetSpotCapacity ::
      Lude.Maybe Lude.Natural,
    instanceFleetId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InstanceFleetModifyConfig' with the minimum fields required to make a request.
--
-- * 'instanceFleetId' - A unique identifier for the instance fleet.
-- * 'targetOnDemandCapacity' - The target capacity of On-Demand units for the instance fleet. For more information see 'InstanceFleetConfig$TargetOnDemandCapacity' .
-- * 'targetSpotCapacity' - The target capacity of Spot units for the instance fleet. For more information, see 'InstanceFleetConfig$TargetSpotCapacity' .
mkInstanceFleetModifyConfig ::
  -- | 'instanceFleetId'
  Lude.Text ->
  InstanceFleetModifyConfig
mkInstanceFleetModifyConfig pInstanceFleetId_ =
  InstanceFleetModifyConfig'
    { targetOnDemandCapacity = Lude.Nothing,
      targetSpotCapacity = Lude.Nothing,
      instanceFleetId = pInstanceFleetId_
    }

-- | The target capacity of On-Demand units for the instance fleet. For more information see 'InstanceFleetConfig$TargetOnDemandCapacity' .
--
-- /Note:/ Consider using 'targetOnDemandCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifmcTargetOnDemandCapacity :: Lens.Lens' InstanceFleetModifyConfig (Lude.Maybe Lude.Natural)
ifmcTargetOnDemandCapacity = Lens.lens (targetOnDemandCapacity :: InstanceFleetModifyConfig -> Lude.Maybe Lude.Natural) (\s a -> s {targetOnDemandCapacity = a} :: InstanceFleetModifyConfig)
{-# DEPRECATED ifmcTargetOnDemandCapacity "Use generic-lens or generic-optics with 'targetOnDemandCapacity' instead." #-}

-- | The target capacity of Spot units for the instance fleet. For more information, see 'InstanceFleetConfig$TargetSpotCapacity' .
--
-- /Note:/ Consider using 'targetSpotCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifmcTargetSpotCapacity :: Lens.Lens' InstanceFleetModifyConfig (Lude.Maybe Lude.Natural)
ifmcTargetSpotCapacity = Lens.lens (targetSpotCapacity :: InstanceFleetModifyConfig -> Lude.Maybe Lude.Natural) (\s a -> s {targetSpotCapacity = a} :: InstanceFleetModifyConfig)
{-# DEPRECATED ifmcTargetSpotCapacity "Use generic-lens or generic-optics with 'targetSpotCapacity' instead." #-}

-- | A unique identifier for the instance fleet.
--
-- /Note:/ Consider using 'instanceFleetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifmcInstanceFleetId :: Lens.Lens' InstanceFleetModifyConfig Lude.Text
ifmcInstanceFleetId = Lens.lens (instanceFleetId :: InstanceFleetModifyConfig -> Lude.Text) (\s a -> s {instanceFleetId = a} :: InstanceFleetModifyConfig)
{-# DEPRECATED ifmcInstanceFleetId "Use generic-lens or generic-optics with 'instanceFleetId' instead." #-}

instance Lude.ToJSON InstanceFleetModifyConfig where
  toJSON InstanceFleetModifyConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("TargetOnDemandCapacity" Lude..=)
              Lude.<$> targetOnDemandCapacity,
            ("TargetSpotCapacity" Lude..=) Lude.<$> targetSpotCapacity,
            Lude.Just ("InstanceFleetId" Lude..= instanceFleetId)
          ]
      )

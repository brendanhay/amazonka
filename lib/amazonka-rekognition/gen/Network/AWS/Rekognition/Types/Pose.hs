{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.Pose
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.Pose
  ( Pose (..),

    -- * Smart constructor
    mkPose,

    -- * Lenses
    pPitch,
    pRoll,
    pYaw,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Indicates the pose of the face as determined by its pitch, roll, and yaw.
--
-- /See:/ 'mkPose' smart constructor.
data Pose = Pose'
  { -- | Value representing the face rotation on the pitch axis.
    pitch :: Core.Maybe Core.Double,
    -- | Value representing the face rotation on the roll axis.
    roll :: Core.Maybe Core.Double,
    -- | Value representing the face rotation on the yaw axis.
    yaw :: Core.Maybe Core.Double
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Pose' value with any optional fields omitted.
mkPose ::
  Pose
mkPose =
  Pose'
    { pitch = Core.Nothing,
      roll = Core.Nothing,
      yaw = Core.Nothing
    }

-- | Value representing the face rotation on the pitch axis.
--
-- /Note:/ Consider using 'pitch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pPitch :: Lens.Lens' Pose (Core.Maybe Core.Double)
pPitch = Lens.field @"pitch"
{-# DEPRECATED pPitch "Use generic-lens or generic-optics with 'pitch' instead." #-}

-- | Value representing the face rotation on the roll axis.
--
-- /Note:/ Consider using 'roll' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pRoll :: Lens.Lens' Pose (Core.Maybe Core.Double)
pRoll = Lens.field @"roll"
{-# DEPRECATED pRoll "Use generic-lens or generic-optics with 'roll' instead." #-}

-- | Value representing the face rotation on the yaw axis.
--
-- /Note:/ Consider using 'yaw' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pYaw :: Lens.Lens' Pose (Core.Maybe Core.Double)
pYaw = Lens.field @"yaw"
{-# DEPRECATED pYaw "Use generic-lens or generic-optics with 'yaw' instead." #-}

instance Core.FromJSON Pose where
  parseJSON =
    Core.withObject "Pose" Core.$
      \x ->
        Pose'
          Core.<$> (x Core..:? "Pitch")
          Core.<*> (x Core..:? "Roll")
          Core.<*> (x Core..:? "Yaw")

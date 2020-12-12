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
    pYaw,
    pRoll,
    pPitch,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Indicates the pose of the face as determined by its pitch, roll, and yaw.
--
-- /See:/ 'mkPose' smart constructor.
data Pose = Pose'
  { yaw :: Lude.Maybe Lude.Double,
    roll :: Lude.Maybe Lude.Double,
    pitch :: Lude.Maybe Lude.Double
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Pose' with the minimum fields required to make a request.
--
-- * 'pitch' - Value representing the face rotation on the pitch axis.
-- * 'roll' - Value representing the face rotation on the roll axis.
-- * 'yaw' - Value representing the face rotation on the yaw axis.
mkPose ::
  Pose
mkPose =
  Pose'
    { yaw = Lude.Nothing,
      roll = Lude.Nothing,
      pitch = Lude.Nothing
    }

-- | Value representing the face rotation on the yaw axis.
--
-- /Note:/ Consider using 'yaw' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pYaw :: Lens.Lens' Pose (Lude.Maybe Lude.Double)
pYaw = Lens.lens (yaw :: Pose -> Lude.Maybe Lude.Double) (\s a -> s {yaw = a} :: Pose)
{-# DEPRECATED pYaw "Use generic-lens or generic-optics with 'yaw' instead." #-}

-- | Value representing the face rotation on the roll axis.
--
-- /Note:/ Consider using 'roll' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pRoll :: Lens.Lens' Pose (Lude.Maybe Lude.Double)
pRoll = Lens.lens (roll :: Pose -> Lude.Maybe Lude.Double) (\s a -> s {roll = a} :: Pose)
{-# DEPRECATED pRoll "Use generic-lens or generic-optics with 'roll' instead." #-}

-- | Value representing the face rotation on the pitch axis.
--
-- /Note:/ Consider using 'pitch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pPitch :: Lens.Lens' Pose (Lude.Maybe Lude.Double)
pPitch = Lens.lens (pitch :: Pose -> Lude.Maybe Lude.Double) (\s a -> s {pitch = a} :: Pose)
{-# DEPRECATED pPitch "Use generic-lens or generic-optics with 'pitch' instead." #-}

instance Lude.FromJSON Pose where
  parseJSON =
    Lude.withObject
      "Pose"
      ( \x ->
          Pose'
            Lude.<$> (x Lude..:? "Yaw")
            Lude.<*> (x Lude..:? "Roll")
            Lude.<*> (x Lude..:? "Pitch")
      )

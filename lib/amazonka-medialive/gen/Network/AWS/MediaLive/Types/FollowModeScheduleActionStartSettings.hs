{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.FollowModeScheduleActionStartSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.FollowModeScheduleActionStartSettings
  ( FollowModeScheduleActionStartSettings (..),

    -- * Smart constructor
    mkFollowModeScheduleActionStartSettings,

    -- * Lenses
    fmsassReferenceActionName,
    fmsassFollowPoint,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.FollowPoint
import qualified Network.AWS.Prelude as Lude

-- | Settings to specify if an action follows another.
--
-- /See:/ 'mkFollowModeScheduleActionStartSettings' smart constructor.
data FollowModeScheduleActionStartSettings = FollowModeScheduleActionStartSettings'
  { referenceActionName ::
      Lude.Text,
    followPoint ::
      FollowPoint
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FollowModeScheduleActionStartSettings' with the minimum fields required to make a request.
--
-- * 'followPoint' - Identifies whether this action starts relative to the start or relative to the end of the reference action.
-- * 'referenceActionName' - The action name of another action that this one refers to.
mkFollowModeScheduleActionStartSettings ::
  -- | 'referenceActionName'
  Lude.Text ->
  -- | 'followPoint'
  FollowPoint ->
  FollowModeScheduleActionStartSettings
mkFollowModeScheduleActionStartSettings
  pReferenceActionName_
  pFollowPoint_ =
    FollowModeScheduleActionStartSettings'
      { referenceActionName =
          pReferenceActionName_,
        followPoint = pFollowPoint_
      }

-- | The action name of another action that this one refers to.
--
-- /Note:/ Consider using 'referenceActionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fmsassReferenceActionName :: Lens.Lens' FollowModeScheduleActionStartSettings Lude.Text
fmsassReferenceActionName = Lens.lens (referenceActionName :: FollowModeScheduleActionStartSettings -> Lude.Text) (\s a -> s {referenceActionName = a} :: FollowModeScheduleActionStartSettings)
{-# DEPRECATED fmsassReferenceActionName "Use generic-lens or generic-optics with 'referenceActionName' instead." #-}

-- | Identifies whether this action starts relative to the start or relative to the end of the reference action.
--
-- /Note:/ Consider using 'followPoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fmsassFollowPoint :: Lens.Lens' FollowModeScheduleActionStartSettings FollowPoint
fmsassFollowPoint = Lens.lens (followPoint :: FollowModeScheduleActionStartSettings -> FollowPoint) (\s a -> s {followPoint = a} :: FollowModeScheduleActionStartSettings)
{-# DEPRECATED fmsassFollowPoint "Use generic-lens or generic-optics with 'followPoint' instead." #-}

instance Lude.FromJSON FollowModeScheduleActionStartSettings where
  parseJSON =
    Lude.withObject
      "FollowModeScheduleActionStartSettings"
      ( \x ->
          FollowModeScheduleActionStartSettings'
            Lude.<$> (x Lude..: "referenceActionName")
            Lude.<*> (x Lude..: "followPoint")
      )

instance Lude.ToJSON FollowModeScheduleActionStartSettings where
  toJSON FollowModeScheduleActionStartSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("referenceActionName" Lude..= referenceActionName),
            Lude.Just ("followPoint" Lude..= followPoint)
          ]
      )

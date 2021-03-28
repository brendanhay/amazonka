{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.FollowModeScheduleActionStartSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.FollowModeScheduleActionStartSettings
  ( FollowModeScheduleActionStartSettings (..)
  -- * Smart constructor
  , mkFollowModeScheduleActionStartSettings
  -- * Lenses
  , fmsassReferenceActionName
  , fmsassFollowPoint
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.FollowPoint as Types
import qualified Network.AWS.Prelude as Core

-- | Settings to specify if an action follows another.
--
-- /See:/ 'mkFollowModeScheduleActionStartSettings' smart constructor.
data FollowModeScheduleActionStartSettings = FollowModeScheduleActionStartSettings'
  { referenceActionName :: Core.Text
    -- ^ The action name of another action that this one refers to.
  , followPoint :: Types.FollowPoint
    -- ^ Identifies whether this action starts relative to the start or relative to the end of the reference action.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FollowModeScheduleActionStartSettings' value with any optional fields omitted.
mkFollowModeScheduleActionStartSettings
    :: Core.Text -- ^ 'referenceActionName'
    -> Types.FollowPoint -- ^ 'followPoint'
    -> FollowModeScheduleActionStartSettings
mkFollowModeScheduleActionStartSettings referenceActionName
  followPoint
  = FollowModeScheduleActionStartSettings'{referenceActionName,
                                           followPoint}

-- | The action name of another action that this one refers to.
--
-- /Note:/ Consider using 'referenceActionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fmsassReferenceActionName :: Lens.Lens' FollowModeScheduleActionStartSettings Core.Text
fmsassReferenceActionName = Lens.field @"referenceActionName"
{-# INLINEABLE fmsassReferenceActionName #-}
{-# DEPRECATED referenceActionName "Use generic-lens or generic-optics with 'referenceActionName' instead"  #-}

-- | Identifies whether this action starts relative to the start or relative to the end of the reference action.
--
-- /Note:/ Consider using 'followPoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fmsassFollowPoint :: Lens.Lens' FollowModeScheduleActionStartSettings Types.FollowPoint
fmsassFollowPoint = Lens.field @"followPoint"
{-# INLINEABLE fmsassFollowPoint #-}
{-# DEPRECATED followPoint "Use generic-lens or generic-optics with 'followPoint' instead"  #-}

instance Core.FromJSON FollowModeScheduleActionStartSettings where
        toJSON FollowModeScheduleActionStartSettings{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("referenceActionName" Core..= referenceActionName),
                  Core.Just ("followPoint" Core..= followPoint)])

instance Core.FromJSON FollowModeScheduleActionStartSettings where
        parseJSON
          = Core.withObject "FollowModeScheduleActionStartSettings" Core.$
              \ x ->
                FollowModeScheduleActionStartSettings' Core.<$>
                  (x Core..: "referenceActionName") Core.<*> x Core..: "followPoint"

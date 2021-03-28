{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.ActivityTypeInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SWF.Types.ActivityTypeInfo
  ( ActivityTypeInfo (..)
  -- * Smart constructor
  , mkActivityTypeInfo
  -- * Lenses
  , atiActivityType
  , atiStatus
  , atiCreationDate
  , atiDeprecationDate
  , atiDescription
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SWF.Types.ActivityType as Types
import qualified Network.AWS.SWF.Types.Description as Types
import qualified Network.AWS.SWF.Types.RegistrationStatus as Types

-- | Detailed information about an activity type.
--
-- /See:/ 'mkActivityTypeInfo' smart constructor.
data ActivityTypeInfo = ActivityTypeInfo'
  { activityType :: Types.ActivityType
    -- ^ The 'ActivityType' type structure representing the activity type.
  , status :: Types.RegistrationStatus
    -- ^ The current status of the activity type.
  , creationDate :: Core.NominalDiffTime
    -- ^ The date and time this activity type was created through 'RegisterActivityType' .
  , deprecationDate :: Core.Maybe Core.NominalDiffTime
    -- ^ If DEPRECATED, the date and time 'DeprecateActivityType' was called.
  , description :: Core.Maybe Types.Description
    -- ^ The description of the activity type provided in 'RegisterActivityType' .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ActivityTypeInfo' value with any optional fields omitted.
mkActivityTypeInfo
    :: Types.ActivityType -- ^ 'activityType'
    -> Types.RegistrationStatus -- ^ 'status'
    -> Core.NominalDiffTime -- ^ 'creationDate'
    -> ActivityTypeInfo
mkActivityTypeInfo activityType status creationDate
  = ActivityTypeInfo'{activityType, status, creationDate,
                      deprecationDate = Core.Nothing, description = Core.Nothing}

-- | The 'ActivityType' type structure representing the activity type.
--
-- /Note:/ Consider using 'activityType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atiActivityType :: Lens.Lens' ActivityTypeInfo Types.ActivityType
atiActivityType = Lens.field @"activityType"
{-# INLINEABLE atiActivityType #-}
{-# DEPRECATED activityType "Use generic-lens or generic-optics with 'activityType' instead"  #-}

-- | The current status of the activity type.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atiStatus :: Lens.Lens' ActivityTypeInfo Types.RegistrationStatus
atiStatus = Lens.field @"status"
{-# INLINEABLE atiStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The date and time this activity type was created through 'RegisterActivityType' .
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atiCreationDate :: Lens.Lens' ActivityTypeInfo Core.NominalDiffTime
atiCreationDate = Lens.field @"creationDate"
{-# INLINEABLE atiCreationDate #-}
{-# DEPRECATED creationDate "Use generic-lens or generic-optics with 'creationDate' instead"  #-}

-- | If DEPRECATED, the date and time 'DeprecateActivityType' was called.
--
-- /Note:/ Consider using 'deprecationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atiDeprecationDate :: Lens.Lens' ActivityTypeInfo (Core.Maybe Core.NominalDiffTime)
atiDeprecationDate = Lens.field @"deprecationDate"
{-# INLINEABLE atiDeprecationDate #-}
{-# DEPRECATED deprecationDate "Use generic-lens or generic-optics with 'deprecationDate' instead"  #-}

-- | The description of the activity type provided in 'RegisterActivityType' .
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atiDescription :: Lens.Lens' ActivityTypeInfo (Core.Maybe Types.Description)
atiDescription = Lens.field @"description"
{-# INLINEABLE atiDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

instance Core.FromJSON ActivityTypeInfo where
        parseJSON
          = Core.withObject "ActivityTypeInfo" Core.$
              \ x ->
                ActivityTypeInfo' Core.<$>
                  (x Core..: "activityType") Core.<*> x Core..: "status" Core.<*>
                    x Core..: "creationDate"
                    Core.<*> x Core..:? "deprecationDate"
                    Core.<*> x Core..:? "description"

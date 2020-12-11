-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.ActivityTypeInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.ActivityTypeInfo
  ( ActivityTypeInfo (..),

    -- * Smart constructor
    mkActivityTypeInfo,

    -- * Lenses
    atiDeprecationDate,
    atiDescription,
    atiActivityType,
    atiStatus,
    atiCreationDate,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SWF.Types.ActivityType
import Network.AWS.SWF.Types.RegistrationStatus

-- | Detailed information about an activity type.
--
-- /See:/ 'mkActivityTypeInfo' smart constructor.
data ActivityTypeInfo = ActivityTypeInfo'
  { deprecationDate ::
      Lude.Maybe Lude.Timestamp,
    description :: Lude.Maybe Lude.Text,
    activityType :: ActivityType,
    status :: RegistrationStatus,
    creationDate :: Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ActivityTypeInfo' with the minimum fields required to make a request.
--
-- * 'activityType' - The 'ActivityType' type structure representing the activity type.
-- * 'creationDate' - The date and time this activity type was created through 'RegisterActivityType' .
-- * 'deprecationDate' - If DEPRECATED, the date and time 'DeprecateActivityType' was called.
-- * 'description' - The description of the activity type provided in 'RegisterActivityType' .
-- * 'status' - The current status of the activity type.
mkActivityTypeInfo ::
  -- | 'activityType'
  ActivityType ->
  -- | 'status'
  RegistrationStatus ->
  -- | 'creationDate'
  Lude.Timestamp ->
  ActivityTypeInfo
mkActivityTypeInfo pActivityType_ pStatus_ pCreationDate_ =
  ActivityTypeInfo'
    { deprecationDate = Lude.Nothing,
      description = Lude.Nothing,
      activityType = pActivityType_,
      status = pStatus_,
      creationDate = pCreationDate_
    }

-- | If DEPRECATED, the date and time 'DeprecateActivityType' was called.
--
-- /Note:/ Consider using 'deprecationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atiDeprecationDate :: Lens.Lens' ActivityTypeInfo (Lude.Maybe Lude.Timestamp)
atiDeprecationDate = Lens.lens (deprecationDate :: ActivityTypeInfo -> Lude.Maybe Lude.Timestamp) (\s a -> s {deprecationDate = a} :: ActivityTypeInfo)
{-# DEPRECATED atiDeprecationDate "Use generic-lens or generic-optics with 'deprecationDate' instead." #-}

-- | The description of the activity type provided in 'RegisterActivityType' .
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atiDescription :: Lens.Lens' ActivityTypeInfo (Lude.Maybe Lude.Text)
atiDescription = Lens.lens (description :: ActivityTypeInfo -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: ActivityTypeInfo)
{-# DEPRECATED atiDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The 'ActivityType' type structure representing the activity type.
--
-- /Note:/ Consider using 'activityType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atiActivityType :: Lens.Lens' ActivityTypeInfo ActivityType
atiActivityType = Lens.lens (activityType :: ActivityTypeInfo -> ActivityType) (\s a -> s {activityType = a} :: ActivityTypeInfo)
{-# DEPRECATED atiActivityType "Use generic-lens or generic-optics with 'activityType' instead." #-}

-- | The current status of the activity type.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atiStatus :: Lens.Lens' ActivityTypeInfo RegistrationStatus
atiStatus = Lens.lens (status :: ActivityTypeInfo -> RegistrationStatus) (\s a -> s {status = a} :: ActivityTypeInfo)
{-# DEPRECATED atiStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The date and time this activity type was created through 'RegisterActivityType' .
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atiCreationDate :: Lens.Lens' ActivityTypeInfo Lude.Timestamp
atiCreationDate = Lens.lens (creationDate :: ActivityTypeInfo -> Lude.Timestamp) (\s a -> s {creationDate = a} :: ActivityTypeInfo)
{-# DEPRECATED atiCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

instance Lude.FromJSON ActivityTypeInfo where
  parseJSON =
    Lude.withObject
      "ActivityTypeInfo"
      ( \x ->
          ActivityTypeInfo'
            Lude.<$> (x Lude..:? "deprecationDate")
            Lude.<*> (x Lude..:? "description")
            Lude.<*> (x Lude..: "activityType")
            Lude.<*> (x Lude..: "status")
            Lude.<*> (x Lude..: "creationDate")
      )

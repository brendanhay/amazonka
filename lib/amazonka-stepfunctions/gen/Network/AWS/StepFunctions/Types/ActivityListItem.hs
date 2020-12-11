-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.ActivityListItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.ActivityListItem
  ( ActivityListItem (..),

    -- * Smart constructor
    mkActivityListItem,

    -- * Lenses
    aliActivityARN,
    aliName,
    aliCreationDate,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains details about an activity.
--
-- /See:/ 'mkActivityListItem' smart constructor.
data ActivityListItem = ActivityListItem'
  { activityARN :: Lude.Text,
    name :: Lude.Text,
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

-- | Creates a value of 'ActivityListItem' with the minimum fields required to make a request.
--
-- * 'activityARN' - The Amazon Resource Name (ARN) that identifies the activity.
-- * 'creationDate' - The date the activity is created.
-- * 'name' - The name of the activity.
--
-- A name must /not/ contain:
--
--     * white space
--
--
--     * brackets @< > { } [ ]@
--
--
--     * wildcard characters @? *@
--
--
--     * special characters @" # % \ ^ | ~ ` $ & , ; : /@
--
--
--     * control characters (@U+0000-001F@ , @U+007F-009F@ )
--
--
-- To enable logging with CloudWatch Logs, the name should only contain 0-9, A-Z, a-z, - and _.
mkActivityListItem ::
  -- | 'activityARN'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  -- | 'creationDate'
  Lude.Timestamp ->
  ActivityListItem
mkActivityListItem pActivityARN_ pName_ pCreationDate_ =
  ActivityListItem'
    { activityARN = pActivityARN_,
      name = pName_,
      creationDate = pCreationDate_
    }

-- | The Amazon Resource Name (ARN) that identifies the activity.
--
-- /Note:/ Consider using 'activityARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aliActivityARN :: Lens.Lens' ActivityListItem Lude.Text
aliActivityARN = Lens.lens (activityARN :: ActivityListItem -> Lude.Text) (\s a -> s {activityARN = a} :: ActivityListItem)
{-# DEPRECATED aliActivityARN "Use generic-lens or generic-optics with 'activityARN' instead." #-}

-- | The name of the activity.
--
-- A name must /not/ contain:
--
--     * white space
--
--
--     * brackets @< > { } [ ]@
--
--
--     * wildcard characters @? *@
--
--
--     * special characters @" # % \ ^ | ~ ` $ & , ; : /@
--
--
--     * control characters (@U+0000-001F@ , @U+007F-009F@ )
--
--
-- To enable logging with CloudWatch Logs, the name should only contain 0-9, A-Z, a-z, - and _.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aliName :: Lens.Lens' ActivityListItem Lude.Text
aliName = Lens.lens (name :: ActivityListItem -> Lude.Text) (\s a -> s {name = a} :: ActivityListItem)
{-# DEPRECATED aliName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The date the activity is created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aliCreationDate :: Lens.Lens' ActivityListItem Lude.Timestamp
aliCreationDate = Lens.lens (creationDate :: ActivityListItem -> Lude.Timestamp) (\s a -> s {creationDate = a} :: ActivityListItem)
{-# DEPRECATED aliCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

instance Lude.FromJSON ActivityListItem where
  parseJSON =
    Lude.withObject
      "ActivityListItem"
      ( \x ->
          ActivityListItem'
            Lude.<$> (x Lude..: "activityArn")
            Lude.<*> (x Lude..: "name")
            Lude.<*> (x Lude..: "creationDate")
      )

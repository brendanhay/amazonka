{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    aliActivityArn,
    aliName,
    aliCreationDate,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.StepFunctions.Types.ActivityArn as Types
import qualified Network.AWS.StepFunctions.Types.Name as Types

-- | Contains details about an activity.
--
-- /See:/ 'mkActivityListItem' smart constructor.
data ActivityListItem = ActivityListItem'
  { -- | The Amazon Resource Name (ARN) that identifies the activity.
    activityArn :: Types.ActivityArn,
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
    name :: Types.Name,
    -- | The date the activity is created.
    creationDate :: Core.NominalDiffTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ActivityListItem' value with any optional fields omitted.
mkActivityListItem ::
  -- | 'activityArn'
  Types.ActivityArn ->
  -- | 'name'
  Types.Name ->
  -- | 'creationDate'
  Core.NominalDiffTime ->
  ActivityListItem
mkActivityListItem activityArn name creationDate =
  ActivityListItem' {activityArn, name, creationDate}

-- | The Amazon Resource Name (ARN) that identifies the activity.
--
-- /Note:/ Consider using 'activityArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aliActivityArn :: Lens.Lens' ActivityListItem Types.ActivityArn
aliActivityArn = Lens.field @"activityArn"
{-# DEPRECATED aliActivityArn "Use generic-lens or generic-optics with 'activityArn' instead." #-}

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
aliName :: Lens.Lens' ActivityListItem Types.Name
aliName = Lens.field @"name"
{-# DEPRECATED aliName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The date the activity is created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aliCreationDate :: Lens.Lens' ActivityListItem Core.NominalDiffTime
aliCreationDate = Lens.field @"creationDate"
{-# DEPRECATED aliCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

instance Core.FromJSON ActivityListItem where
  parseJSON =
    Core.withObject "ActivityListItem" Core.$
      \x ->
        ActivityListItem'
          Core.<$> (x Core..: "activityArn")
          Core.<*> (x Core..: "name")
          Core.<*> (x Core..: "creationDate")

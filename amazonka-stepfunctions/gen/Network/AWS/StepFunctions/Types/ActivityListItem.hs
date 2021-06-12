{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.ActivityListItem
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.ActivityListItem where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains details about an activity.
--
-- /See:/ 'newActivityListItem' smart constructor.
data ActivityListItem = ActivityListItem'
  { -- | The Amazon Resource Name (ARN) that identifies the activity.
    activityArn :: Core.Text,
    -- | The name of the activity.
    --
    -- A name must /not/ contain:
    --
    -- -   white space
    --
    -- -   brackets @\< > { } [ ]@
    --
    -- -   wildcard characters @? *@
    --
    -- -   special characters @\" # % \\ ^ | ~ \` $ & , ; : \/@
    --
    -- -   control characters (@U+0000-001F@, @U+007F-009F@)
    --
    -- To enable logging with CloudWatch Logs, the name should only contain
    -- 0-9, A-Z, a-z, - and _.
    name :: Core.Text,
    -- | The date the activity is created.
    creationDate :: Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ActivityListItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'activityArn', 'activityListItem_activityArn' - The Amazon Resource Name (ARN) that identifies the activity.
--
-- 'name', 'activityListItem_name' - The name of the activity.
--
-- A name must /not/ contain:
--
-- -   white space
--
-- -   brackets @\< > { } [ ]@
--
-- -   wildcard characters @? *@
--
-- -   special characters @\" # % \\ ^ | ~ \` $ & , ; : \/@
--
-- -   control characters (@U+0000-001F@, @U+007F-009F@)
--
-- To enable logging with CloudWatch Logs, the name should only contain
-- 0-9, A-Z, a-z, - and _.
--
-- 'creationDate', 'activityListItem_creationDate' - The date the activity is created.
newActivityListItem ::
  -- | 'activityArn'
  Core.Text ->
  -- | 'name'
  Core.Text ->
  -- | 'creationDate'
  Core.UTCTime ->
  ActivityListItem
newActivityListItem
  pActivityArn_
  pName_
  pCreationDate_ =
    ActivityListItem'
      { activityArn = pActivityArn_,
        name = pName_,
        creationDate = Core._Time Lens.# pCreationDate_
      }

-- | The Amazon Resource Name (ARN) that identifies the activity.
activityListItem_activityArn :: Lens.Lens' ActivityListItem Core.Text
activityListItem_activityArn = Lens.lens (\ActivityListItem' {activityArn} -> activityArn) (\s@ActivityListItem' {} a -> s {activityArn = a} :: ActivityListItem)

-- | The name of the activity.
--
-- A name must /not/ contain:
--
-- -   white space
--
-- -   brackets @\< > { } [ ]@
--
-- -   wildcard characters @? *@
--
-- -   special characters @\" # % \\ ^ | ~ \` $ & , ; : \/@
--
-- -   control characters (@U+0000-001F@, @U+007F-009F@)
--
-- To enable logging with CloudWatch Logs, the name should only contain
-- 0-9, A-Z, a-z, - and _.
activityListItem_name :: Lens.Lens' ActivityListItem Core.Text
activityListItem_name = Lens.lens (\ActivityListItem' {name} -> name) (\s@ActivityListItem' {} a -> s {name = a} :: ActivityListItem)

-- | The date the activity is created.
activityListItem_creationDate :: Lens.Lens' ActivityListItem Core.UTCTime
activityListItem_creationDate = Lens.lens (\ActivityListItem' {creationDate} -> creationDate) (\s@ActivityListItem' {} a -> s {creationDate = a} :: ActivityListItem) Core.. Core._Time

instance Core.FromJSON ActivityListItem where
  parseJSON =
    Core.withObject
      "ActivityListItem"
      ( \x ->
          ActivityListItem'
            Core.<$> (x Core..: "activityArn")
            Core.<*> (x Core..: "name")
            Core.<*> (x Core..: "creationDate")
      )

instance Core.Hashable ActivityListItem

instance Core.NFData ActivityListItem

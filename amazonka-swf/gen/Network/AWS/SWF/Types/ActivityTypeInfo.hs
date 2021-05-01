{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.SWF.Types.ActivityTypeInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.ActivityTypeInfo where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SWF.Types.ActivityType
import Network.AWS.SWF.Types.RegistrationStatus

-- | Detailed information about an activity type.
--
-- /See:/ 'newActivityTypeInfo' smart constructor.
data ActivityTypeInfo = ActivityTypeInfo'
  { -- | If DEPRECATED, the date and time DeprecateActivityType was called.
    deprecationDate :: Prelude.Maybe Prelude.POSIX,
    -- | The description of the activity type provided in RegisterActivityType.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ActivityType type structure representing the activity type.
    activityType :: ActivityType,
    -- | The current status of the activity type.
    status :: RegistrationStatus,
    -- | The date and time this activity type was created through
    -- RegisterActivityType.
    creationDate :: Prelude.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ActivityTypeInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deprecationDate', 'activityTypeInfo_deprecationDate' - If DEPRECATED, the date and time DeprecateActivityType was called.
--
-- 'description', 'activityTypeInfo_description' - The description of the activity type provided in RegisterActivityType.
--
-- 'activityType', 'activityTypeInfo_activityType' - The ActivityType type structure representing the activity type.
--
-- 'status', 'activityTypeInfo_status' - The current status of the activity type.
--
-- 'creationDate', 'activityTypeInfo_creationDate' - The date and time this activity type was created through
-- RegisterActivityType.
newActivityTypeInfo ::
  -- | 'activityType'
  ActivityType ->
  -- | 'status'
  RegistrationStatus ->
  -- | 'creationDate'
  Prelude.UTCTime ->
  ActivityTypeInfo
newActivityTypeInfo
  pActivityType_
  pStatus_
  pCreationDate_ =
    ActivityTypeInfo'
      { deprecationDate =
          Prelude.Nothing,
        description = Prelude.Nothing,
        activityType = pActivityType_,
        status = pStatus_,
        creationDate = Prelude._Time Lens.# pCreationDate_
      }

-- | If DEPRECATED, the date and time DeprecateActivityType was called.
activityTypeInfo_deprecationDate :: Lens.Lens' ActivityTypeInfo (Prelude.Maybe Prelude.UTCTime)
activityTypeInfo_deprecationDate = Lens.lens (\ActivityTypeInfo' {deprecationDate} -> deprecationDate) (\s@ActivityTypeInfo' {} a -> s {deprecationDate = a} :: ActivityTypeInfo) Prelude.. Lens.mapping Prelude._Time

-- | The description of the activity type provided in RegisterActivityType.
activityTypeInfo_description :: Lens.Lens' ActivityTypeInfo (Prelude.Maybe Prelude.Text)
activityTypeInfo_description = Lens.lens (\ActivityTypeInfo' {description} -> description) (\s@ActivityTypeInfo' {} a -> s {description = a} :: ActivityTypeInfo)

-- | The ActivityType type structure representing the activity type.
activityTypeInfo_activityType :: Lens.Lens' ActivityTypeInfo ActivityType
activityTypeInfo_activityType = Lens.lens (\ActivityTypeInfo' {activityType} -> activityType) (\s@ActivityTypeInfo' {} a -> s {activityType = a} :: ActivityTypeInfo)

-- | The current status of the activity type.
activityTypeInfo_status :: Lens.Lens' ActivityTypeInfo RegistrationStatus
activityTypeInfo_status = Lens.lens (\ActivityTypeInfo' {status} -> status) (\s@ActivityTypeInfo' {} a -> s {status = a} :: ActivityTypeInfo)

-- | The date and time this activity type was created through
-- RegisterActivityType.
activityTypeInfo_creationDate :: Lens.Lens' ActivityTypeInfo Prelude.UTCTime
activityTypeInfo_creationDate = Lens.lens (\ActivityTypeInfo' {creationDate} -> creationDate) (\s@ActivityTypeInfo' {} a -> s {creationDate = a} :: ActivityTypeInfo) Prelude.. Prelude._Time

instance Prelude.FromJSON ActivityTypeInfo where
  parseJSON =
    Prelude.withObject
      "ActivityTypeInfo"
      ( \x ->
          ActivityTypeInfo'
            Prelude.<$> (x Prelude..:? "deprecationDate")
            Prelude.<*> (x Prelude..:? "description")
            Prelude.<*> (x Prelude..: "activityType")
            Prelude.<*> (x Prelude..: "status")
            Prelude.<*> (x Prelude..: "creationDate")
      )

instance Prelude.Hashable ActivityTypeInfo

instance Prelude.NFData ActivityTypeInfo

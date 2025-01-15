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
-- Module      : Amazonka.SWF.Types.ActivityTypeInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SWF.Types.ActivityTypeInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SWF.Types.ActivityType
import Amazonka.SWF.Types.RegistrationStatus

-- | Detailed information about an activity type.
--
-- /See:/ 'newActivityTypeInfo' smart constructor.
data ActivityTypeInfo = ActivityTypeInfo'
  { -- | If DEPRECATED, the date and time DeprecateActivityType was called.
    deprecationDate :: Prelude.Maybe Data.POSIX,
    -- | The description of the activity type provided in RegisterActivityType.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ActivityType type structure representing the activity type.
    activityType :: ActivityType,
    -- | The current status of the activity type.
    status :: RegistrationStatus,
    -- | The date and time this activity type was created through
    -- RegisterActivityType.
    creationDate :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
        creationDate = Data._Time Lens.# pCreationDate_
      }

-- | If DEPRECATED, the date and time DeprecateActivityType was called.
activityTypeInfo_deprecationDate :: Lens.Lens' ActivityTypeInfo (Prelude.Maybe Prelude.UTCTime)
activityTypeInfo_deprecationDate = Lens.lens (\ActivityTypeInfo' {deprecationDate} -> deprecationDate) (\s@ActivityTypeInfo' {} a -> s {deprecationDate = a} :: ActivityTypeInfo) Prelude.. Lens.mapping Data._Time

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
activityTypeInfo_creationDate = Lens.lens (\ActivityTypeInfo' {creationDate} -> creationDate) (\s@ActivityTypeInfo' {} a -> s {creationDate = a} :: ActivityTypeInfo) Prelude.. Data._Time

instance Data.FromJSON ActivityTypeInfo where
  parseJSON =
    Data.withObject
      "ActivityTypeInfo"
      ( \x ->
          ActivityTypeInfo'
            Prelude.<$> (x Data..:? "deprecationDate")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..: "activityType")
            Prelude.<*> (x Data..: "status")
            Prelude.<*> (x Data..: "creationDate")
      )

instance Prelude.Hashable ActivityTypeInfo where
  hashWithSalt _salt ActivityTypeInfo' {..} =
    _salt
      `Prelude.hashWithSalt` deprecationDate
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` activityType
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` creationDate

instance Prelude.NFData ActivityTypeInfo where
  rnf ActivityTypeInfo' {..} =
    Prelude.rnf deprecationDate `Prelude.seq`
      Prelude.rnf description `Prelude.seq`
        Prelude.rnf activityType `Prelude.seq`
          Prelude.rnf status `Prelude.seq`
            Prelude.rnf creationDate

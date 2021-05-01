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
-- Module      : Network.AWS.StepFunctions.Types.ActivityScheduleFailedEventDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.ActivityScheduleFailedEventDetails where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains details about an activity schedule failure that occurred during
-- an execution.
--
-- /See:/ 'newActivityScheduleFailedEventDetails' smart constructor.
data ActivityScheduleFailedEventDetails = ActivityScheduleFailedEventDetails'
  { -- | A more detailed explanation of the cause of the failure.
    cause :: Prelude.Maybe (Prelude.Sensitive Prelude.Text),
    -- | The error code of the failure.
    error :: Prelude.Maybe (Prelude.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ActivityScheduleFailedEventDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cause', 'activityScheduleFailedEventDetails_cause' - A more detailed explanation of the cause of the failure.
--
-- 'error', 'activityScheduleFailedEventDetails_error' - The error code of the failure.
newActivityScheduleFailedEventDetails ::
  ActivityScheduleFailedEventDetails
newActivityScheduleFailedEventDetails =
  ActivityScheduleFailedEventDetails'
    { cause =
        Prelude.Nothing,
      error = Prelude.Nothing
    }

-- | A more detailed explanation of the cause of the failure.
activityScheduleFailedEventDetails_cause :: Lens.Lens' ActivityScheduleFailedEventDetails (Prelude.Maybe Prelude.Text)
activityScheduleFailedEventDetails_cause = Lens.lens (\ActivityScheduleFailedEventDetails' {cause} -> cause) (\s@ActivityScheduleFailedEventDetails' {} a -> s {cause = a} :: ActivityScheduleFailedEventDetails) Prelude.. Lens.mapping Prelude._Sensitive

-- | The error code of the failure.
activityScheduleFailedEventDetails_error :: Lens.Lens' ActivityScheduleFailedEventDetails (Prelude.Maybe Prelude.Text)
activityScheduleFailedEventDetails_error = Lens.lens (\ActivityScheduleFailedEventDetails' {error} -> error) (\s@ActivityScheduleFailedEventDetails' {} a -> s {error = a} :: ActivityScheduleFailedEventDetails) Prelude.. Lens.mapping Prelude._Sensitive

instance
  Prelude.FromJSON
    ActivityScheduleFailedEventDetails
  where
  parseJSON =
    Prelude.withObject
      "ActivityScheduleFailedEventDetails"
      ( \x ->
          ActivityScheduleFailedEventDetails'
            Prelude.<$> (x Prelude..:? "cause")
            Prelude.<*> (x Prelude..:? "error")
      )

instance
  Prelude.Hashable
    ActivityScheduleFailedEventDetails

instance
  Prelude.NFData
    ActivityScheduleFailedEventDetails

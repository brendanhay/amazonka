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
-- Module      : Amazonka.StepFunctions.Types.ActivityScheduleFailedEventDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.StepFunctions.Types.ActivityScheduleFailedEventDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains details about an activity schedule failure that occurred during
-- an execution.
--
-- /See:/ 'newActivityScheduleFailedEventDetails' smart constructor.
data ActivityScheduleFailedEventDetails = ActivityScheduleFailedEventDetails'
  { -- | The error code of the failure.
    error :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | A more detailed explanation of the cause of the failure.
    cause :: Prelude.Maybe (Core.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ActivityScheduleFailedEventDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'error', 'activityScheduleFailedEventDetails_error' - The error code of the failure.
--
-- 'cause', 'activityScheduleFailedEventDetails_cause' - A more detailed explanation of the cause of the failure.
newActivityScheduleFailedEventDetails ::
  ActivityScheduleFailedEventDetails
newActivityScheduleFailedEventDetails =
  ActivityScheduleFailedEventDetails'
    { error =
        Prelude.Nothing,
      cause = Prelude.Nothing
    }

-- | The error code of the failure.
activityScheduleFailedEventDetails_error :: Lens.Lens' ActivityScheduleFailedEventDetails (Prelude.Maybe Prelude.Text)
activityScheduleFailedEventDetails_error = Lens.lens (\ActivityScheduleFailedEventDetails' {error} -> error) (\s@ActivityScheduleFailedEventDetails' {} a -> s {error = a} :: ActivityScheduleFailedEventDetails) Prelude.. Lens.mapping Core._Sensitive

-- | A more detailed explanation of the cause of the failure.
activityScheduleFailedEventDetails_cause :: Lens.Lens' ActivityScheduleFailedEventDetails (Prelude.Maybe Prelude.Text)
activityScheduleFailedEventDetails_cause = Lens.lens (\ActivityScheduleFailedEventDetails' {cause} -> cause) (\s@ActivityScheduleFailedEventDetails' {} a -> s {cause = a} :: ActivityScheduleFailedEventDetails) Prelude.. Lens.mapping Core._Sensitive

instance
  Core.FromJSON
    ActivityScheduleFailedEventDetails
  where
  parseJSON =
    Core.withObject
      "ActivityScheduleFailedEventDetails"
      ( \x ->
          ActivityScheduleFailedEventDetails'
            Prelude.<$> (x Core..:? "error")
            Prelude.<*> (x Core..:? "cause")
      )

instance
  Prelude.Hashable
    ActivityScheduleFailedEventDetails
  where
  hashWithSalt
    _salt
    ActivityScheduleFailedEventDetails' {..} =
      _salt `Prelude.hashWithSalt` error
        `Prelude.hashWithSalt` cause

instance
  Prelude.NFData
    ActivityScheduleFailedEventDetails
  where
  rnf ActivityScheduleFailedEventDetails' {..} =
    Prelude.rnf error `Prelude.seq` Prelude.rnf cause

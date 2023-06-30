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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.StepFunctions.Types.ActivityScheduleFailedEventDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains details about an activity schedule failure that occurred during
-- an execution.
--
-- /See:/ 'newActivityScheduleFailedEventDetails' smart constructor.
data ActivityScheduleFailedEventDetails = ActivityScheduleFailedEventDetails'
  { -- | A more detailed explanation of the cause of the failure.
    cause :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The error code of the failure.
    error :: Prelude.Maybe (Data.Sensitive Prelude.Text)
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
activityScheduleFailedEventDetails_cause = Lens.lens (\ActivityScheduleFailedEventDetails' {cause} -> cause) (\s@ActivityScheduleFailedEventDetails' {} a -> s {cause = a} :: ActivityScheduleFailedEventDetails) Prelude.. Lens.mapping Data._Sensitive

-- | The error code of the failure.
activityScheduleFailedEventDetails_error :: Lens.Lens' ActivityScheduleFailedEventDetails (Prelude.Maybe Prelude.Text)
activityScheduleFailedEventDetails_error = Lens.lens (\ActivityScheduleFailedEventDetails' {error} -> error) (\s@ActivityScheduleFailedEventDetails' {} a -> s {error = a} :: ActivityScheduleFailedEventDetails) Prelude.. Lens.mapping Data._Sensitive

instance
  Data.FromJSON
    ActivityScheduleFailedEventDetails
  where
  parseJSON =
    Data.withObject
      "ActivityScheduleFailedEventDetails"
      ( \x ->
          ActivityScheduleFailedEventDetails'
            Prelude.<$> (x Data..:? "cause")
            Prelude.<*> (x Data..:? "error")
      )

instance
  Prelude.Hashable
    ActivityScheduleFailedEventDetails
  where
  hashWithSalt
    _salt
    ActivityScheduleFailedEventDetails' {..} =
      _salt
        `Prelude.hashWithSalt` cause
        `Prelude.hashWithSalt` error

instance
  Prelude.NFData
    ActivityScheduleFailedEventDetails
  where
  rnf ActivityScheduleFailedEventDetails' {..} =
    Prelude.rnf cause `Prelude.seq` Prelude.rnf error

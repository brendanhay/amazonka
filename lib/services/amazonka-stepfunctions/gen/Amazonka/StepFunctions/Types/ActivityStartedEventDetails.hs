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
-- Module      : Amazonka.StepFunctions.Types.ActivityStartedEventDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.StepFunctions.Types.ActivityStartedEventDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains details about the start of an activity during an execution.
--
-- /See:/ 'newActivityStartedEventDetails' smart constructor.
data ActivityStartedEventDetails = ActivityStartedEventDetails'
  { -- | The name of the worker that the task is assigned to. These names are
    -- provided by the workers when calling GetActivityTask.
    workerName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ActivityStartedEventDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'workerName', 'activityStartedEventDetails_workerName' - The name of the worker that the task is assigned to. These names are
-- provided by the workers when calling GetActivityTask.
newActivityStartedEventDetails ::
  ActivityStartedEventDetails
newActivityStartedEventDetails =
  ActivityStartedEventDetails'
    { workerName =
        Prelude.Nothing
    }

-- | The name of the worker that the task is assigned to. These names are
-- provided by the workers when calling GetActivityTask.
activityStartedEventDetails_workerName :: Lens.Lens' ActivityStartedEventDetails (Prelude.Maybe Prelude.Text)
activityStartedEventDetails_workerName = Lens.lens (\ActivityStartedEventDetails' {workerName} -> workerName) (\s@ActivityStartedEventDetails' {} a -> s {workerName = a} :: ActivityStartedEventDetails)

instance Data.FromJSON ActivityStartedEventDetails where
  parseJSON =
    Data.withObject
      "ActivityStartedEventDetails"
      ( \x ->
          ActivityStartedEventDetails'
            Prelude.<$> (x Data..:? "workerName")
      )

instance Prelude.Hashable ActivityStartedEventDetails where
  hashWithSalt _salt ActivityStartedEventDetails' {..} =
    _salt `Prelude.hashWithSalt` workerName

instance Prelude.NFData ActivityStartedEventDetails where
  rnf ActivityStartedEventDetails' {..} =
    Prelude.rnf workerName

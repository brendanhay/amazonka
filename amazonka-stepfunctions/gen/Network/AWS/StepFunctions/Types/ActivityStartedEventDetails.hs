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
-- Module      : Network.AWS.StepFunctions.Types.ActivityStartedEventDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.ActivityStartedEventDetails where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains details about the start of an activity during an execution.
--
-- /See:/ 'newActivityStartedEventDetails' smart constructor.
data ActivityStartedEventDetails = ActivityStartedEventDetails'
  { -- | The name of the worker that the task is assigned to. These names are
    -- provided by the workers when calling GetActivityTask.
    workerName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON ActivityStartedEventDetails where
  parseJSON =
    Prelude.withObject
      "ActivityStartedEventDetails"
      ( \x ->
          ActivityStartedEventDetails'
            Prelude.<$> (x Prelude..:? "workerName")
      )

instance Prelude.Hashable ActivityStartedEventDetails

instance Prelude.NFData ActivityStartedEventDetails

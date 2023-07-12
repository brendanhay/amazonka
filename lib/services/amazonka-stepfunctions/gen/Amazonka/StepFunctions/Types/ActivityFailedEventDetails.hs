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
-- Module      : Amazonka.StepFunctions.Types.ActivityFailedEventDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.StepFunctions.Types.ActivityFailedEventDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains details about an activity that failed during an execution.
--
-- /See:/ 'newActivityFailedEventDetails' smart constructor.
data ActivityFailedEventDetails = ActivityFailedEventDetails'
  { -- | A more detailed explanation of the cause of the failure.
    cause :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The error code of the failure.
    error :: Prelude.Maybe (Data.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ActivityFailedEventDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cause', 'activityFailedEventDetails_cause' - A more detailed explanation of the cause of the failure.
--
-- 'error', 'activityFailedEventDetails_error' - The error code of the failure.
newActivityFailedEventDetails ::
  ActivityFailedEventDetails
newActivityFailedEventDetails =
  ActivityFailedEventDetails'
    { cause =
        Prelude.Nothing,
      error = Prelude.Nothing
    }

-- | A more detailed explanation of the cause of the failure.
activityFailedEventDetails_cause :: Lens.Lens' ActivityFailedEventDetails (Prelude.Maybe Prelude.Text)
activityFailedEventDetails_cause = Lens.lens (\ActivityFailedEventDetails' {cause} -> cause) (\s@ActivityFailedEventDetails' {} a -> s {cause = a} :: ActivityFailedEventDetails) Prelude.. Lens.mapping Data._Sensitive

-- | The error code of the failure.
activityFailedEventDetails_error :: Lens.Lens' ActivityFailedEventDetails (Prelude.Maybe Prelude.Text)
activityFailedEventDetails_error = Lens.lens (\ActivityFailedEventDetails' {error} -> error) (\s@ActivityFailedEventDetails' {} a -> s {error = a} :: ActivityFailedEventDetails) Prelude.. Lens.mapping Data._Sensitive

instance Data.FromJSON ActivityFailedEventDetails where
  parseJSON =
    Data.withObject
      "ActivityFailedEventDetails"
      ( \x ->
          ActivityFailedEventDetails'
            Prelude.<$> (x Data..:? "cause")
            Prelude.<*> (x Data..:? "error")
      )

instance Prelude.Hashable ActivityFailedEventDetails where
  hashWithSalt _salt ActivityFailedEventDetails' {..} =
    _salt
      `Prelude.hashWithSalt` cause
      `Prelude.hashWithSalt` error

instance Prelude.NFData ActivityFailedEventDetails where
  rnf ActivityFailedEventDetails' {..} =
    Prelude.rnf cause `Prelude.seq` Prelude.rnf error

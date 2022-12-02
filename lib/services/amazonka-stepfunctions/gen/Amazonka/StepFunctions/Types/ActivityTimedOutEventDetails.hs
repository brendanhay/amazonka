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
-- Module      : Amazonka.StepFunctions.Types.ActivityTimedOutEventDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.StepFunctions.Types.ActivityTimedOutEventDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains details about an activity timeout that occurred during an
-- execution.
--
-- /See:/ 'newActivityTimedOutEventDetails' smart constructor.
data ActivityTimedOutEventDetails = ActivityTimedOutEventDetails'
  { -- | The error code of the failure.
    error :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | A more detailed explanation of the cause of the timeout.
    cause :: Prelude.Maybe (Data.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ActivityTimedOutEventDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'error', 'activityTimedOutEventDetails_error' - The error code of the failure.
--
-- 'cause', 'activityTimedOutEventDetails_cause' - A more detailed explanation of the cause of the timeout.
newActivityTimedOutEventDetails ::
  ActivityTimedOutEventDetails
newActivityTimedOutEventDetails =
  ActivityTimedOutEventDetails'
    { error =
        Prelude.Nothing,
      cause = Prelude.Nothing
    }

-- | The error code of the failure.
activityTimedOutEventDetails_error :: Lens.Lens' ActivityTimedOutEventDetails (Prelude.Maybe Prelude.Text)
activityTimedOutEventDetails_error = Lens.lens (\ActivityTimedOutEventDetails' {error} -> error) (\s@ActivityTimedOutEventDetails' {} a -> s {error = a} :: ActivityTimedOutEventDetails) Prelude.. Lens.mapping Data._Sensitive

-- | A more detailed explanation of the cause of the timeout.
activityTimedOutEventDetails_cause :: Lens.Lens' ActivityTimedOutEventDetails (Prelude.Maybe Prelude.Text)
activityTimedOutEventDetails_cause = Lens.lens (\ActivityTimedOutEventDetails' {cause} -> cause) (\s@ActivityTimedOutEventDetails' {} a -> s {cause = a} :: ActivityTimedOutEventDetails) Prelude.. Lens.mapping Data._Sensitive

instance Data.FromJSON ActivityTimedOutEventDetails where
  parseJSON =
    Data.withObject
      "ActivityTimedOutEventDetails"
      ( \x ->
          ActivityTimedOutEventDetails'
            Prelude.<$> (x Data..:? "error")
            Prelude.<*> (x Data..:? "cause")
      )

instance
  Prelude.Hashable
    ActivityTimedOutEventDetails
  where
  hashWithSalt _salt ActivityTimedOutEventDetails' {..} =
    _salt `Prelude.hashWithSalt` error
      `Prelude.hashWithSalt` cause

instance Prelude.NFData ActivityTimedOutEventDetails where
  rnf ActivityTimedOutEventDetails' {..} =
    Prelude.rnf error `Prelude.seq` Prelude.rnf cause

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
-- Module      : Amazonka.StepFunctions.Types.LambdaFunctionScheduleFailedEventDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.StepFunctions.Types.LambdaFunctionScheduleFailedEventDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains details about a failed Lambda function schedule event that
-- occurred during an execution.
--
-- /See:/ 'newLambdaFunctionScheduleFailedEventDetails' smart constructor.
data LambdaFunctionScheduleFailedEventDetails = LambdaFunctionScheduleFailedEventDetails'
  { -- | The error code of the failure.
    error :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | A more detailed explanation of the cause of the failure.
    cause :: Prelude.Maybe (Core.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LambdaFunctionScheduleFailedEventDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'error', 'lambdaFunctionScheduleFailedEventDetails_error' - The error code of the failure.
--
-- 'cause', 'lambdaFunctionScheduleFailedEventDetails_cause' - A more detailed explanation of the cause of the failure.
newLambdaFunctionScheduleFailedEventDetails ::
  LambdaFunctionScheduleFailedEventDetails
newLambdaFunctionScheduleFailedEventDetails =
  LambdaFunctionScheduleFailedEventDetails'
    { error =
        Prelude.Nothing,
      cause = Prelude.Nothing
    }

-- | The error code of the failure.
lambdaFunctionScheduleFailedEventDetails_error :: Lens.Lens' LambdaFunctionScheduleFailedEventDetails (Prelude.Maybe Prelude.Text)
lambdaFunctionScheduleFailedEventDetails_error = Lens.lens (\LambdaFunctionScheduleFailedEventDetails' {error} -> error) (\s@LambdaFunctionScheduleFailedEventDetails' {} a -> s {error = a} :: LambdaFunctionScheduleFailedEventDetails) Prelude.. Lens.mapping Core._Sensitive

-- | A more detailed explanation of the cause of the failure.
lambdaFunctionScheduleFailedEventDetails_cause :: Lens.Lens' LambdaFunctionScheduleFailedEventDetails (Prelude.Maybe Prelude.Text)
lambdaFunctionScheduleFailedEventDetails_cause = Lens.lens (\LambdaFunctionScheduleFailedEventDetails' {cause} -> cause) (\s@LambdaFunctionScheduleFailedEventDetails' {} a -> s {cause = a} :: LambdaFunctionScheduleFailedEventDetails) Prelude.. Lens.mapping Core._Sensitive

instance
  Core.FromJSON
    LambdaFunctionScheduleFailedEventDetails
  where
  parseJSON =
    Core.withObject
      "LambdaFunctionScheduleFailedEventDetails"
      ( \x ->
          LambdaFunctionScheduleFailedEventDetails'
            Prelude.<$> (x Core..:? "error")
            Prelude.<*> (x Core..:? "cause")
      )

instance
  Prelude.Hashable
    LambdaFunctionScheduleFailedEventDetails
  where
  hashWithSalt
    _salt
    LambdaFunctionScheduleFailedEventDetails' {..} =
      _salt `Prelude.hashWithSalt` error
        `Prelude.hashWithSalt` cause

instance
  Prelude.NFData
    LambdaFunctionScheduleFailedEventDetails
  where
  rnf LambdaFunctionScheduleFailedEventDetails' {..} =
    Prelude.rnf error `Prelude.seq` Prelude.rnf cause

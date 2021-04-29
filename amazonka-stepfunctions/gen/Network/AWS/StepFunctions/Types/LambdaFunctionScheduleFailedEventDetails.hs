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
-- Module      : Network.AWS.StepFunctions.Types.LambdaFunctionScheduleFailedEventDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.LambdaFunctionScheduleFailedEventDetails where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains details about a failed lambda function schedule event that
-- occurred during an execution.
--
-- /See:/ 'newLambdaFunctionScheduleFailedEventDetails' smart constructor.
data LambdaFunctionScheduleFailedEventDetails = LambdaFunctionScheduleFailedEventDetails'
  { -- | A more detailed explanation of the cause of the failure.
    cause :: Prelude.Maybe (Prelude.Sensitive Prelude.Text),
    -- | The error code of the failure.
    error :: Prelude.Maybe (Prelude.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'LambdaFunctionScheduleFailedEventDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cause', 'lambdaFunctionScheduleFailedEventDetails_cause' - A more detailed explanation of the cause of the failure.
--
-- 'error', 'lambdaFunctionScheduleFailedEventDetails_error' - The error code of the failure.
newLambdaFunctionScheduleFailedEventDetails ::
  LambdaFunctionScheduleFailedEventDetails
newLambdaFunctionScheduleFailedEventDetails =
  LambdaFunctionScheduleFailedEventDetails'
    { cause =
        Prelude.Nothing,
      error = Prelude.Nothing
    }

-- | A more detailed explanation of the cause of the failure.
lambdaFunctionScheduleFailedEventDetails_cause :: Lens.Lens' LambdaFunctionScheduleFailedEventDetails (Prelude.Maybe Prelude.Text)
lambdaFunctionScheduleFailedEventDetails_cause = Lens.lens (\LambdaFunctionScheduleFailedEventDetails' {cause} -> cause) (\s@LambdaFunctionScheduleFailedEventDetails' {} a -> s {cause = a} :: LambdaFunctionScheduleFailedEventDetails) Prelude.. Lens.mapping Prelude._Sensitive

-- | The error code of the failure.
lambdaFunctionScheduleFailedEventDetails_error :: Lens.Lens' LambdaFunctionScheduleFailedEventDetails (Prelude.Maybe Prelude.Text)
lambdaFunctionScheduleFailedEventDetails_error = Lens.lens (\LambdaFunctionScheduleFailedEventDetails' {error} -> error) (\s@LambdaFunctionScheduleFailedEventDetails' {} a -> s {error = a} :: LambdaFunctionScheduleFailedEventDetails) Prelude.. Lens.mapping Prelude._Sensitive

instance
  Prelude.FromJSON
    LambdaFunctionScheduleFailedEventDetails
  where
  parseJSON =
    Prelude.withObject
      "LambdaFunctionScheduleFailedEventDetails"
      ( \x ->
          LambdaFunctionScheduleFailedEventDetails'
            Prelude.<$> (x Prelude..:? "cause")
            Prelude.<*> (x Prelude..:? "error")
      )

instance
  Prelude.Hashable
    LambdaFunctionScheduleFailedEventDetails

instance
  Prelude.NFData
    LambdaFunctionScheduleFailedEventDetails

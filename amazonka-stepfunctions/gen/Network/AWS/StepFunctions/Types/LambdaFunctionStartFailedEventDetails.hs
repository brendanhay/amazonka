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
-- Module      : Network.AWS.StepFunctions.Types.LambdaFunctionStartFailedEventDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.LambdaFunctionStartFailedEventDetails where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains details about a lambda function that failed to start during an
-- execution.
--
-- /See:/ 'newLambdaFunctionStartFailedEventDetails' smart constructor.
data LambdaFunctionStartFailedEventDetails = LambdaFunctionStartFailedEventDetails'
  { -- | A more detailed explanation of the cause of the failure.
    cause :: Core.Maybe (Core.Sensitive Core.Text),
    -- | The error code of the failure.
    error :: Core.Maybe (Core.Sensitive Core.Text)
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'LambdaFunctionStartFailedEventDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cause', 'lambdaFunctionStartFailedEventDetails_cause' - A more detailed explanation of the cause of the failure.
--
-- 'error', 'lambdaFunctionStartFailedEventDetails_error' - The error code of the failure.
newLambdaFunctionStartFailedEventDetails ::
  LambdaFunctionStartFailedEventDetails
newLambdaFunctionStartFailedEventDetails =
  LambdaFunctionStartFailedEventDetails'
    { cause =
        Core.Nothing,
      error = Core.Nothing
    }

-- | A more detailed explanation of the cause of the failure.
lambdaFunctionStartFailedEventDetails_cause :: Lens.Lens' LambdaFunctionStartFailedEventDetails (Core.Maybe Core.Text)
lambdaFunctionStartFailedEventDetails_cause = Lens.lens (\LambdaFunctionStartFailedEventDetails' {cause} -> cause) (\s@LambdaFunctionStartFailedEventDetails' {} a -> s {cause = a} :: LambdaFunctionStartFailedEventDetails) Core.. Lens.mapping Core._Sensitive

-- | The error code of the failure.
lambdaFunctionStartFailedEventDetails_error :: Lens.Lens' LambdaFunctionStartFailedEventDetails (Core.Maybe Core.Text)
lambdaFunctionStartFailedEventDetails_error = Lens.lens (\LambdaFunctionStartFailedEventDetails' {error} -> error) (\s@LambdaFunctionStartFailedEventDetails' {} a -> s {error = a} :: LambdaFunctionStartFailedEventDetails) Core.. Lens.mapping Core._Sensitive

instance
  Core.FromJSON
    LambdaFunctionStartFailedEventDetails
  where
  parseJSON =
    Core.withObject
      "LambdaFunctionStartFailedEventDetails"
      ( \x ->
          LambdaFunctionStartFailedEventDetails'
            Core.<$> (x Core..:? "cause") Core.<*> (x Core..:? "error")
      )

instance
  Core.Hashable
    LambdaFunctionStartFailedEventDetails

instance
  Core.NFData
    LambdaFunctionStartFailedEventDetails

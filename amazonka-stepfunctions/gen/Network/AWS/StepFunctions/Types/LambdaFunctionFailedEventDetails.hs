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
-- Module      : Network.AWS.StepFunctions.Types.LambdaFunctionFailedEventDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.LambdaFunctionFailedEventDetails where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains details about a lambda function that failed during an
-- execution.
--
-- /See:/ 'newLambdaFunctionFailedEventDetails' smart constructor.
data LambdaFunctionFailedEventDetails = LambdaFunctionFailedEventDetails'
  { -- | A more detailed explanation of the cause of the failure.
    cause :: Core.Maybe (Core.Sensitive Core.Text),
    -- | The error code of the failure.
    error :: Core.Maybe (Core.Sensitive Core.Text)
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'LambdaFunctionFailedEventDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cause', 'lambdaFunctionFailedEventDetails_cause' - A more detailed explanation of the cause of the failure.
--
-- 'error', 'lambdaFunctionFailedEventDetails_error' - The error code of the failure.
newLambdaFunctionFailedEventDetails ::
  LambdaFunctionFailedEventDetails
newLambdaFunctionFailedEventDetails =
  LambdaFunctionFailedEventDetails'
    { cause =
        Core.Nothing,
      error = Core.Nothing
    }

-- | A more detailed explanation of the cause of the failure.
lambdaFunctionFailedEventDetails_cause :: Lens.Lens' LambdaFunctionFailedEventDetails (Core.Maybe Core.Text)
lambdaFunctionFailedEventDetails_cause = Lens.lens (\LambdaFunctionFailedEventDetails' {cause} -> cause) (\s@LambdaFunctionFailedEventDetails' {} a -> s {cause = a} :: LambdaFunctionFailedEventDetails) Core.. Lens.mapping Core._Sensitive

-- | The error code of the failure.
lambdaFunctionFailedEventDetails_error :: Lens.Lens' LambdaFunctionFailedEventDetails (Core.Maybe Core.Text)
lambdaFunctionFailedEventDetails_error = Lens.lens (\LambdaFunctionFailedEventDetails' {error} -> error) (\s@LambdaFunctionFailedEventDetails' {} a -> s {error = a} :: LambdaFunctionFailedEventDetails) Core.. Lens.mapping Core._Sensitive

instance
  Core.FromJSON
    LambdaFunctionFailedEventDetails
  where
  parseJSON =
    Core.withObject
      "LambdaFunctionFailedEventDetails"
      ( \x ->
          LambdaFunctionFailedEventDetails'
            Core.<$> (x Core..:? "cause") Core.<*> (x Core..:? "error")
      )

instance
  Core.Hashable
    LambdaFunctionFailedEventDetails

instance Core.NFData LambdaFunctionFailedEventDetails

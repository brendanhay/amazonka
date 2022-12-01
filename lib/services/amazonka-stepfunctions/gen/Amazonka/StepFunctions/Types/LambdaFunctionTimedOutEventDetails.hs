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
-- Module      : Amazonka.StepFunctions.Types.LambdaFunctionTimedOutEventDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.StepFunctions.Types.LambdaFunctionTimedOutEventDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains details about a Lambda function timeout that occurred during an
-- execution.
--
-- /See:/ 'newLambdaFunctionTimedOutEventDetails' smart constructor.
data LambdaFunctionTimedOutEventDetails = LambdaFunctionTimedOutEventDetails'
  { -- | The error code of the failure.
    error :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | A more detailed explanation of the cause of the timeout.
    cause :: Prelude.Maybe (Core.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LambdaFunctionTimedOutEventDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'error', 'lambdaFunctionTimedOutEventDetails_error' - The error code of the failure.
--
-- 'cause', 'lambdaFunctionTimedOutEventDetails_cause' - A more detailed explanation of the cause of the timeout.
newLambdaFunctionTimedOutEventDetails ::
  LambdaFunctionTimedOutEventDetails
newLambdaFunctionTimedOutEventDetails =
  LambdaFunctionTimedOutEventDetails'
    { error =
        Prelude.Nothing,
      cause = Prelude.Nothing
    }

-- | The error code of the failure.
lambdaFunctionTimedOutEventDetails_error :: Lens.Lens' LambdaFunctionTimedOutEventDetails (Prelude.Maybe Prelude.Text)
lambdaFunctionTimedOutEventDetails_error = Lens.lens (\LambdaFunctionTimedOutEventDetails' {error} -> error) (\s@LambdaFunctionTimedOutEventDetails' {} a -> s {error = a} :: LambdaFunctionTimedOutEventDetails) Prelude.. Lens.mapping Core._Sensitive

-- | A more detailed explanation of the cause of the timeout.
lambdaFunctionTimedOutEventDetails_cause :: Lens.Lens' LambdaFunctionTimedOutEventDetails (Prelude.Maybe Prelude.Text)
lambdaFunctionTimedOutEventDetails_cause = Lens.lens (\LambdaFunctionTimedOutEventDetails' {cause} -> cause) (\s@LambdaFunctionTimedOutEventDetails' {} a -> s {cause = a} :: LambdaFunctionTimedOutEventDetails) Prelude.. Lens.mapping Core._Sensitive

instance
  Core.FromJSON
    LambdaFunctionTimedOutEventDetails
  where
  parseJSON =
    Core.withObject
      "LambdaFunctionTimedOutEventDetails"
      ( \x ->
          LambdaFunctionTimedOutEventDetails'
            Prelude.<$> (x Core..:? "error")
            Prelude.<*> (x Core..:? "cause")
      )

instance
  Prelude.Hashable
    LambdaFunctionTimedOutEventDetails
  where
  hashWithSalt
    _salt
    LambdaFunctionTimedOutEventDetails' {..} =
      _salt `Prelude.hashWithSalt` error
        `Prelude.hashWithSalt` cause

instance
  Prelude.NFData
    LambdaFunctionTimedOutEventDetails
  where
  rnf LambdaFunctionTimedOutEventDetails' {..} =
    Prelude.rnf error `Prelude.seq` Prelude.rnf cause

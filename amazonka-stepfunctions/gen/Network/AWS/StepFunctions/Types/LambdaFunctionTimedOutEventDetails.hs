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
-- Module      : Network.AWS.StepFunctions.Types.LambdaFunctionTimedOutEventDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.LambdaFunctionTimedOutEventDetails where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains details about a lambda function timeout that occurred during an
-- execution.
--
-- /See:/ 'newLambdaFunctionTimedOutEventDetails' smart constructor.
data LambdaFunctionTimedOutEventDetails = LambdaFunctionTimedOutEventDetails'
  { -- | A more detailed explanation of the cause of the timeout.
    cause :: Prelude.Maybe (Prelude.Sensitive Prelude.Text),
    -- | The error code of the failure.
    error :: Prelude.Maybe (Prelude.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'LambdaFunctionTimedOutEventDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cause', 'lambdaFunctionTimedOutEventDetails_cause' - A more detailed explanation of the cause of the timeout.
--
-- 'error', 'lambdaFunctionTimedOutEventDetails_error' - The error code of the failure.
newLambdaFunctionTimedOutEventDetails ::
  LambdaFunctionTimedOutEventDetails
newLambdaFunctionTimedOutEventDetails =
  LambdaFunctionTimedOutEventDetails'
    { cause =
        Prelude.Nothing,
      error = Prelude.Nothing
    }

-- | A more detailed explanation of the cause of the timeout.
lambdaFunctionTimedOutEventDetails_cause :: Lens.Lens' LambdaFunctionTimedOutEventDetails (Prelude.Maybe Prelude.Text)
lambdaFunctionTimedOutEventDetails_cause = Lens.lens (\LambdaFunctionTimedOutEventDetails' {cause} -> cause) (\s@LambdaFunctionTimedOutEventDetails' {} a -> s {cause = a} :: LambdaFunctionTimedOutEventDetails) Prelude.. Lens.mapping Prelude._Sensitive

-- | The error code of the failure.
lambdaFunctionTimedOutEventDetails_error :: Lens.Lens' LambdaFunctionTimedOutEventDetails (Prelude.Maybe Prelude.Text)
lambdaFunctionTimedOutEventDetails_error = Lens.lens (\LambdaFunctionTimedOutEventDetails' {error} -> error) (\s@LambdaFunctionTimedOutEventDetails' {} a -> s {error = a} :: LambdaFunctionTimedOutEventDetails) Prelude.. Lens.mapping Prelude._Sensitive

instance
  Prelude.FromJSON
    LambdaFunctionTimedOutEventDetails
  where
  parseJSON =
    Prelude.withObject
      "LambdaFunctionTimedOutEventDetails"
      ( \x ->
          LambdaFunctionTimedOutEventDetails'
            Prelude.<$> (x Prelude..:? "cause")
            Prelude.<*> (x Prelude..:? "error")
      )

instance
  Prelude.Hashable
    LambdaFunctionTimedOutEventDetails

instance
  Prelude.NFData
    LambdaFunctionTimedOutEventDetails

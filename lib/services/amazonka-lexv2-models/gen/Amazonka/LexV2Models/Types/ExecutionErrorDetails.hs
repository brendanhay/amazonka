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
-- Module      : Amazonka.LexV2Models.Types.ExecutionErrorDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.ExecutionErrorDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details about an error in an execution of a test set.
--
-- /See:/ 'newExecutionErrorDetails' smart constructor.
data ExecutionErrorDetails = ExecutionErrorDetails'
  { -- | The error code for the error.
    errorCode :: Prelude.Text,
    -- | The message describing the error.
    errorMessage :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExecutionErrorDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorCode', 'executionErrorDetails_errorCode' - The error code for the error.
--
-- 'errorMessage', 'executionErrorDetails_errorMessage' - The message describing the error.
newExecutionErrorDetails ::
  -- | 'errorCode'
  Prelude.Text ->
  -- | 'errorMessage'
  Prelude.Text ->
  ExecutionErrorDetails
newExecutionErrorDetails pErrorCode_ pErrorMessage_ =
  ExecutionErrorDetails'
    { errorCode = pErrorCode_,
      errorMessage = pErrorMessage_
    }

-- | The error code for the error.
executionErrorDetails_errorCode :: Lens.Lens' ExecutionErrorDetails Prelude.Text
executionErrorDetails_errorCode = Lens.lens (\ExecutionErrorDetails' {errorCode} -> errorCode) (\s@ExecutionErrorDetails' {} a -> s {errorCode = a} :: ExecutionErrorDetails)

-- | The message describing the error.
executionErrorDetails_errorMessage :: Lens.Lens' ExecutionErrorDetails Prelude.Text
executionErrorDetails_errorMessage = Lens.lens (\ExecutionErrorDetails' {errorMessage} -> errorMessage) (\s@ExecutionErrorDetails' {} a -> s {errorMessage = a} :: ExecutionErrorDetails)

instance Data.FromJSON ExecutionErrorDetails where
  parseJSON =
    Data.withObject
      "ExecutionErrorDetails"
      ( \x ->
          ExecutionErrorDetails'
            Prelude.<$> (x Data..: "errorCode")
            Prelude.<*> (x Data..: "errorMessage")
      )

instance Prelude.Hashable ExecutionErrorDetails where
  hashWithSalt _salt ExecutionErrorDetails' {..} =
    _salt
      `Prelude.hashWithSalt` errorCode
      `Prelude.hashWithSalt` errorMessage

instance Prelude.NFData ExecutionErrorDetails where
  rnf ExecutionErrorDetails' {..} =
    Prelude.rnf errorCode
      `Prelude.seq` Prelude.rnf errorMessage

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
-- Module      : Amazonka.AppStream.Types.StackError
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppStream.Types.StackError where

import Amazonka.AppStream.Types.StackErrorCode
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a stack error.
--
-- /See:/ 'newStackError' smart constructor.
data StackError = StackError'
  { -- | The error code.
    errorCode :: Prelude.Maybe StackErrorCode,
    -- | The error message.
    errorMessage :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StackError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorCode', 'stackError_errorCode' - The error code.
--
-- 'errorMessage', 'stackError_errorMessage' - The error message.
newStackError ::
  StackError
newStackError =
  StackError'
    { errorCode = Prelude.Nothing,
      errorMessage = Prelude.Nothing
    }

-- | The error code.
stackError_errorCode :: Lens.Lens' StackError (Prelude.Maybe StackErrorCode)
stackError_errorCode = Lens.lens (\StackError' {errorCode} -> errorCode) (\s@StackError' {} a -> s {errorCode = a} :: StackError)

-- | The error message.
stackError_errorMessage :: Lens.Lens' StackError (Prelude.Maybe Prelude.Text)
stackError_errorMessage = Lens.lens (\StackError' {errorMessage} -> errorMessage) (\s@StackError' {} a -> s {errorMessage = a} :: StackError)

instance Data.FromJSON StackError where
  parseJSON =
    Data.withObject
      "StackError"
      ( \x ->
          StackError'
            Prelude.<$> (x Data..:? "ErrorCode")
            Prelude.<*> (x Data..:? "ErrorMessage")
      )

instance Prelude.Hashable StackError where
  hashWithSalt _salt StackError' {..} =
    _salt
      `Prelude.hashWithSalt` errorCode
      `Prelude.hashWithSalt` errorMessage

instance Prelude.NFData StackError where
  rnf StackError' {..} =
    Prelude.rnf errorCode `Prelude.seq`
      Prelude.rnf errorMessage

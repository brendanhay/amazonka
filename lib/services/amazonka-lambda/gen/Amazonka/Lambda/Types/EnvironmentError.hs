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
-- Module      : Amazonka.Lambda.Types.EnvironmentError
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lambda.Types.EnvironmentError where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Error messages for environment variables that couldn\'t be applied.
--
-- /See:/ 'newEnvironmentError' smart constructor.
data EnvironmentError = EnvironmentError'
  { -- | The error code.
    errorCode :: Prelude.Maybe Prelude.Text,
    -- | The error message.
    message :: Prelude.Maybe (Data.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnvironmentError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorCode', 'environmentError_errorCode' - The error code.
--
-- 'message', 'environmentError_message' - The error message.
newEnvironmentError ::
  EnvironmentError
newEnvironmentError =
  EnvironmentError'
    { errorCode = Prelude.Nothing,
      message = Prelude.Nothing
    }

-- | The error code.
environmentError_errorCode :: Lens.Lens' EnvironmentError (Prelude.Maybe Prelude.Text)
environmentError_errorCode = Lens.lens (\EnvironmentError' {errorCode} -> errorCode) (\s@EnvironmentError' {} a -> s {errorCode = a} :: EnvironmentError)

-- | The error message.
environmentError_message :: Lens.Lens' EnvironmentError (Prelude.Maybe Prelude.Text)
environmentError_message = Lens.lens (\EnvironmentError' {message} -> message) (\s@EnvironmentError' {} a -> s {message = a} :: EnvironmentError) Prelude.. Lens.mapping Data._Sensitive

instance Data.FromJSON EnvironmentError where
  parseJSON =
    Data.withObject
      "EnvironmentError"
      ( \x ->
          EnvironmentError'
            Prelude.<$> (x Data..:? "ErrorCode")
            Prelude.<*> (x Data..:? "Message")
      )

instance Prelude.Hashable EnvironmentError where
  hashWithSalt _salt EnvironmentError' {..} =
    _salt `Prelude.hashWithSalt` errorCode
      `Prelude.hashWithSalt` message

instance Prelude.NFData EnvironmentError where
  rnf EnvironmentError' {..} =
    Prelude.rnf errorCode
      `Prelude.seq` Prelude.rnf message

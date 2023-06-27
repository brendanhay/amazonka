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
-- Module      : Amazonka.Lambda.Types.RuntimeVersionError
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lambda.Types.RuntimeVersionError where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Any error returned when the runtime version information for the function
-- could not be retrieved.
--
-- /See:/ 'newRuntimeVersionError' smart constructor.
data RuntimeVersionError = RuntimeVersionError'
  { -- | The error code.
    errorCode :: Prelude.Maybe Prelude.Text,
    -- | The error message.
    message :: Prelude.Maybe (Data.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RuntimeVersionError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorCode', 'runtimeVersionError_errorCode' - The error code.
--
-- 'message', 'runtimeVersionError_message' - The error message.
newRuntimeVersionError ::
  RuntimeVersionError
newRuntimeVersionError =
  RuntimeVersionError'
    { errorCode = Prelude.Nothing,
      message = Prelude.Nothing
    }

-- | The error code.
runtimeVersionError_errorCode :: Lens.Lens' RuntimeVersionError (Prelude.Maybe Prelude.Text)
runtimeVersionError_errorCode = Lens.lens (\RuntimeVersionError' {errorCode} -> errorCode) (\s@RuntimeVersionError' {} a -> s {errorCode = a} :: RuntimeVersionError)

-- | The error message.
runtimeVersionError_message :: Lens.Lens' RuntimeVersionError (Prelude.Maybe Prelude.Text)
runtimeVersionError_message = Lens.lens (\RuntimeVersionError' {message} -> message) (\s@RuntimeVersionError' {} a -> s {message = a} :: RuntimeVersionError) Prelude.. Lens.mapping Data._Sensitive

instance Data.FromJSON RuntimeVersionError where
  parseJSON =
    Data.withObject
      "RuntimeVersionError"
      ( \x ->
          RuntimeVersionError'
            Prelude.<$> (x Data..:? "ErrorCode")
            Prelude.<*> (x Data..:? "Message")
      )

instance Prelude.Hashable RuntimeVersionError where
  hashWithSalt _salt RuntimeVersionError' {..} =
    _salt
      `Prelude.hashWithSalt` errorCode
      `Prelude.hashWithSalt` message

instance Prelude.NFData RuntimeVersionError where
  rnf RuntimeVersionError' {..} =
    Prelude.rnf errorCode
      `Prelude.seq` Prelude.rnf message

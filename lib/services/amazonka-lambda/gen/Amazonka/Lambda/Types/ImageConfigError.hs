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
-- Module      : Amazonka.Lambda.Types.ImageConfigError
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lambda.Types.ImageConfigError where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Error response to @GetFunctionConfiguration@.
--
-- /See:/ 'newImageConfigError' smart constructor.
data ImageConfigError = ImageConfigError'
  { -- | Error code.
    errorCode :: Prelude.Maybe Prelude.Text,
    -- | Error message.
    message :: Prelude.Maybe (Data.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImageConfigError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorCode', 'imageConfigError_errorCode' - Error code.
--
-- 'message', 'imageConfigError_message' - Error message.
newImageConfigError ::
  ImageConfigError
newImageConfigError =
  ImageConfigError'
    { errorCode = Prelude.Nothing,
      message = Prelude.Nothing
    }

-- | Error code.
imageConfigError_errorCode :: Lens.Lens' ImageConfigError (Prelude.Maybe Prelude.Text)
imageConfigError_errorCode = Lens.lens (\ImageConfigError' {errorCode} -> errorCode) (\s@ImageConfigError' {} a -> s {errorCode = a} :: ImageConfigError)

-- | Error message.
imageConfigError_message :: Lens.Lens' ImageConfigError (Prelude.Maybe Prelude.Text)
imageConfigError_message = Lens.lens (\ImageConfigError' {message} -> message) (\s@ImageConfigError' {} a -> s {message = a} :: ImageConfigError) Prelude.. Lens.mapping Data._Sensitive

instance Data.FromJSON ImageConfigError where
  parseJSON =
    Data.withObject
      "ImageConfigError"
      ( \x ->
          ImageConfigError'
            Prelude.<$> (x Data..:? "ErrorCode")
            Prelude.<*> (x Data..:? "Message")
      )

instance Prelude.Hashable ImageConfigError where
  hashWithSalt _salt ImageConfigError' {..} =
    _salt
      `Prelude.hashWithSalt` errorCode
      `Prelude.hashWithSalt` message

instance Prelude.NFData ImageConfigError where
  rnf ImageConfigError' {..} =
    Prelude.rnf errorCode
      `Prelude.seq` Prelude.rnf message

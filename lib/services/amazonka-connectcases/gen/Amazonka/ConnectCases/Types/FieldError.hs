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
-- Module      : Amazonka.ConnectCases.Types.FieldError
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ConnectCases.Types.FieldError where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Object for errors on fields.
--
-- /See:/ 'newFieldError' smart constructor.
data FieldError = FieldError'
  { -- | The error message from getting a field.
    message :: Prelude.Maybe Prelude.Text,
    -- | The error code from getting a field.
    errorCode :: Prelude.Text,
    -- | The field identifier that caused the error.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FieldError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'fieldError_message' - The error message from getting a field.
--
-- 'errorCode', 'fieldError_errorCode' - The error code from getting a field.
--
-- 'id', 'fieldError_id' - The field identifier that caused the error.
newFieldError ::
  -- | 'errorCode'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  FieldError
newFieldError pErrorCode_ pId_ =
  FieldError'
    { message = Prelude.Nothing,
      errorCode = pErrorCode_,
      id = pId_
    }

-- | The error message from getting a field.
fieldError_message :: Lens.Lens' FieldError (Prelude.Maybe Prelude.Text)
fieldError_message = Lens.lens (\FieldError' {message} -> message) (\s@FieldError' {} a -> s {message = a} :: FieldError)

-- | The error code from getting a field.
fieldError_errorCode :: Lens.Lens' FieldError Prelude.Text
fieldError_errorCode = Lens.lens (\FieldError' {errorCode} -> errorCode) (\s@FieldError' {} a -> s {errorCode = a} :: FieldError)

-- | The field identifier that caused the error.
fieldError_id :: Lens.Lens' FieldError Prelude.Text
fieldError_id = Lens.lens (\FieldError' {id} -> id) (\s@FieldError' {} a -> s {id = a} :: FieldError)

instance Data.FromJSON FieldError where
  parseJSON =
    Data.withObject
      "FieldError"
      ( \x ->
          FieldError'
            Prelude.<$> (x Data..:? "message")
            Prelude.<*> (x Data..: "errorCode")
            Prelude.<*> (x Data..: "id")
      )

instance Prelude.Hashable FieldError where
  hashWithSalt _salt FieldError' {..} =
    _salt
      `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` errorCode
      `Prelude.hashWithSalt` id

instance Prelude.NFData FieldError where
  rnf FieldError' {..} =
    Prelude.rnf message
      `Prelude.seq` Prelude.rnf errorCode
      `Prelude.seq` Prelude.rnf id

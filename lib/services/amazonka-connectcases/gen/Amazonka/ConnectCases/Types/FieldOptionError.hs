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
-- Module      : Amazonka.ConnectCases.Types.FieldOptionError
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ConnectCases.Types.FieldOptionError where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Object for field Options errors.
--
-- /See:/ 'newFieldOptionError' smart constructor.
data FieldOptionError = FieldOptionError'
  { -- | Error code from creating or updating field option.
    errorCode :: Prelude.Text,
    -- | Error message from creating or updating field option.
    message :: Prelude.Text,
    -- | The field option value that caused the error.
    value :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FieldOptionError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorCode', 'fieldOptionError_errorCode' - Error code from creating or updating field option.
--
-- 'message', 'fieldOptionError_message' - Error message from creating or updating field option.
--
-- 'value', 'fieldOptionError_value' - The field option value that caused the error.
newFieldOptionError ::
  -- | 'errorCode'
  Prelude.Text ->
  -- | 'message'
  Prelude.Text ->
  -- | 'value'
  Prelude.Text ->
  FieldOptionError
newFieldOptionError pErrorCode_ pMessage_ pValue_ =
  FieldOptionError'
    { errorCode = pErrorCode_,
      message = pMessage_,
      value = pValue_
    }

-- | Error code from creating or updating field option.
fieldOptionError_errorCode :: Lens.Lens' FieldOptionError Prelude.Text
fieldOptionError_errorCode = Lens.lens (\FieldOptionError' {errorCode} -> errorCode) (\s@FieldOptionError' {} a -> s {errorCode = a} :: FieldOptionError)

-- | Error message from creating or updating field option.
fieldOptionError_message :: Lens.Lens' FieldOptionError Prelude.Text
fieldOptionError_message = Lens.lens (\FieldOptionError' {message} -> message) (\s@FieldOptionError' {} a -> s {message = a} :: FieldOptionError)

-- | The field option value that caused the error.
fieldOptionError_value :: Lens.Lens' FieldOptionError Prelude.Text
fieldOptionError_value = Lens.lens (\FieldOptionError' {value} -> value) (\s@FieldOptionError' {} a -> s {value = a} :: FieldOptionError)

instance Data.FromJSON FieldOptionError where
  parseJSON =
    Data.withObject
      "FieldOptionError"
      ( \x ->
          FieldOptionError'
            Prelude.<$> (x Data..: "errorCode")
            Prelude.<*> (x Data..: "message")
            Prelude.<*> (x Data..: "value")
      )

instance Prelude.Hashable FieldOptionError where
  hashWithSalt _salt FieldOptionError' {..} =
    _salt `Prelude.hashWithSalt` errorCode
      `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` value

instance Prelude.NFData FieldOptionError where
  rnf FieldOptionError' {..} =
    Prelude.rnf errorCode
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf value

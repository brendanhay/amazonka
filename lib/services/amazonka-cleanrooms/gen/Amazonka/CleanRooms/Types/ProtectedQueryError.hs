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
-- Module      : Amazonka.CleanRooms.Types.ProtectedQueryError
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CleanRooms.Types.ProtectedQueryError where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details of errors thrown by the protected query.
--
-- /See:/ 'newProtectedQueryError' smart constructor.
data ProtectedQueryError = ProtectedQueryError'
  { -- | A description of why the query failed.
    message :: Prelude.Text,
    -- | An error code for the error.
    code :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProtectedQueryError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'protectedQueryError_message' - A description of why the query failed.
--
-- 'code', 'protectedQueryError_code' - An error code for the error.
newProtectedQueryError ::
  -- | 'message'
  Prelude.Text ->
  -- | 'code'
  Prelude.Text ->
  ProtectedQueryError
newProtectedQueryError pMessage_ pCode_ =
  ProtectedQueryError'
    { message = pMessage_,
      code = pCode_
    }

-- | A description of why the query failed.
protectedQueryError_message :: Lens.Lens' ProtectedQueryError Prelude.Text
protectedQueryError_message = Lens.lens (\ProtectedQueryError' {message} -> message) (\s@ProtectedQueryError' {} a -> s {message = a} :: ProtectedQueryError)

-- | An error code for the error.
protectedQueryError_code :: Lens.Lens' ProtectedQueryError Prelude.Text
protectedQueryError_code = Lens.lens (\ProtectedQueryError' {code} -> code) (\s@ProtectedQueryError' {} a -> s {code = a} :: ProtectedQueryError)

instance Data.FromJSON ProtectedQueryError where
  parseJSON =
    Data.withObject
      "ProtectedQueryError"
      ( \x ->
          ProtectedQueryError'
            Prelude.<$> (x Data..: "message")
            Prelude.<*> (x Data..: "code")
      )

instance Prelude.Hashable ProtectedQueryError where
  hashWithSalt _salt ProtectedQueryError' {..} =
    _salt
      `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` code

instance Prelude.NFData ProtectedQueryError where
  rnf ProtectedQueryError' {..} =
    Prelude.rnf message `Prelude.seq` Prelude.rnf code

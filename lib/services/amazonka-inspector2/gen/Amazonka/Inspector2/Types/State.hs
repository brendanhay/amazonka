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
-- Module      : Amazonka.Inspector2.Types.State
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.State where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector2.Types.ErrorCode
import Amazonka.Inspector2.Types.Status
import qualified Amazonka.Prelude as Prelude

-- | An object that described the state of Amazon Inspector scans for an
-- account.
--
-- /See:/ 'newState' smart constructor.
data State = State'
  { -- | The error code explaining why the account failed to enable Amazon
    -- Inspector.
    errorCode :: ErrorCode,
    -- | The error message received when the account failed to enable Amazon
    -- Inspector.
    errorMessage :: Prelude.Text,
    -- | The status of Amazon Inspector for the account.
    status :: Status
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'State' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorCode', 'state_errorCode' - The error code explaining why the account failed to enable Amazon
-- Inspector.
--
-- 'errorMessage', 'state_errorMessage' - The error message received when the account failed to enable Amazon
-- Inspector.
--
-- 'status', 'state_status' - The status of Amazon Inspector for the account.
newState ::
  -- | 'errorCode'
  ErrorCode ->
  -- | 'errorMessage'
  Prelude.Text ->
  -- | 'status'
  Status ->
  State
newState pErrorCode_ pErrorMessage_ pStatus_ =
  State'
    { errorCode = pErrorCode_,
      errorMessage = pErrorMessage_,
      status = pStatus_
    }

-- | The error code explaining why the account failed to enable Amazon
-- Inspector.
state_errorCode :: Lens.Lens' State ErrorCode
state_errorCode = Lens.lens (\State' {errorCode} -> errorCode) (\s@State' {} a -> s {errorCode = a} :: State)

-- | The error message received when the account failed to enable Amazon
-- Inspector.
state_errorMessage :: Lens.Lens' State Prelude.Text
state_errorMessage = Lens.lens (\State' {errorMessage} -> errorMessage) (\s@State' {} a -> s {errorMessage = a} :: State)

-- | The status of Amazon Inspector for the account.
state_status :: Lens.Lens' State Status
state_status = Lens.lens (\State' {status} -> status) (\s@State' {} a -> s {status = a} :: State)

instance Data.FromJSON State where
  parseJSON =
    Data.withObject
      "State"
      ( \x ->
          State'
            Prelude.<$> (x Data..: "errorCode")
            Prelude.<*> (x Data..: "errorMessage")
            Prelude.<*> (x Data..: "status")
      )

instance Prelude.Hashable State where
  hashWithSalt _salt State' {..} =
    _salt
      `Prelude.hashWithSalt` errorCode
      `Prelude.hashWithSalt` errorMessage
      `Prelude.hashWithSalt` status

instance Prelude.NFData State where
  rnf State' {..} =
    Prelude.rnf errorCode
      `Prelude.seq` Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf status

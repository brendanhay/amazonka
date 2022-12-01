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
-- Module      : Amazonka.Inspector2.Types.FreeTrialInfoError
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.FreeTrialInfoError where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Inspector2.Types.FreeTrialInfoErrorCode
import qualified Amazonka.Prelude as Prelude

-- | Information about an error received while accessing free trail data for
-- an account.
--
-- /See:/ 'newFreeTrialInfoError' smart constructor.
data FreeTrialInfoError = FreeTrialInfoError'
  { -- | The account associated with the Amazon Inspector free trial information.
    accountId :: Prelude.Text,
    -- | The error code.
    code :: FreeTrialInfoErrorCode,
    -- | The error message returned.
    message :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FreeTrialInfoError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'freeTrialInfoError_accountId' - The account associated with the Amazon Inspector free trial information.
--
-- 'code', 'freeTrialInfoError_code' - The error code.
--
-- 'message', 'freeTrialInfoError_message' - The error message returned.
newFreeTrialInfoError ::
  -- | 'accountId'
  Prelude.Text ->
  -- | 'code'
  FreeTrialInfoErrorCode ->
  -- | 'message'
  Prelude.Text ->
  FreeTrialInfoError
newFreeTrialInfoError pAccountId_ pCode_ pMessage_ =
  FreeTrialInfoError'
    { accountId = pAccountId_,
      code = pCode_,
      message = pMessage_
    }

-- | The account associated with the Amazon Inspector free trial information.
freeTrialInfoError_accountId :: Lens.Lens' FreeTrialInfoError Prelude.Text
freeTrialInfoError_accountId = Lens.lens (\FreeTrialInfoError' {accountId} -> accountId) (\s@FreeTrialInfoError' {} a -> s {accountId = a} :: FreeTrialInfoError)

-- | The error code.
freeTrialInfoError_code :: Lens.Lens' FreeTrialInfoError FreeTrialInfoErrorCode
freeTrialInfoError_code = Lens.lens (\FreeTrialInfoError' {code} -> code) (\s@FreeTrialInfoError' {} a -> s {code = a} :: FreeTrialInfoError)

-- | The error message returned.
freeTrialInfoError_message :: Lens.Lens' FreeTrialInfoError Prelude.Text
freeTrialInfoError_message = Lens.lens (\FreeTrialInfoError' {message} -> message) (\s@FreeTrialInfoError' {} a -> s {message = a} :: FreeTrialInfoError)

instance Core.FromJSON FreeTrialInfoError where
  parseJSON =
    Core.withObject
      "FreeTrialInfoError"
      ( \x ->
          FreeTrialInfoError'
            Prelude.<$> (x Core..: "accountId")
            Prelude.<*> (x Core..: "code")
            Prelude.<*> (x Core..: "message")
      )

instance Prelude.Hashable FreeTrialInfoError where
  hashWithSalt _salt FreeTrialInfoError' {..} =
    _salt `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` code
      `Prelude.hashWithSalt` message

instance Prelude.NFData FreeTrialInfoError where
  rnf FreeTrialInfoError' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf code
      `Prelude.seq` Prelude.rnf message

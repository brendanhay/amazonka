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
-- Module      : Amazonka.Inspector2.Types.FailedAccount
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.FailedAccount where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector2.Types.ErrorCode
import Amazonka.Inspector2.Types.ResourceStatus
import Amazonka.Inspector2.Types.Status
import qualified Amazonka.Prelude as Prelude

-- | An object with details on why an account failed to enable Amazon
-- Inspector.
--
-- /See:/ 'newFailedAccount' smart constructor.
data FailedAccount = FailedAccount'
  { -- | An object detailing which resources Amazon Inspector is enabled to scan
    -- for the account.
    resourceStatus :: Prelude.Maybe ResourceStatus,
    -- | The status of Amazon Inspector for the account.
    status :: Prelude.Maybe Status,
    -- | The Amazon Web Services account ID.
    accountId :: Prelude.Text,
    -- | The error code explaining why the account failed to enable Amazon
    -- Inspector.
    errorCode :: ErrorCode,
    -- | The error message received when the account failed to enable Amazon
    -- Inspector.
    errorMessage :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FailedAccount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceStatus', 'failedAccount_resourceStatus' - An object detailing which resources Amazon Inspector is enabled to scan
-- for the account.
--
-- 'status', 'failedAccount_status' - The status of Amazon Inspector for the account.
--
-- 'accountId', 'failedAccount_accountId' - The Amazon Web Services account ID.
--
-- 'errorCode', 'failedAccount_errorCode' - The error code explaining why the account failed to enable Amazon
-- Inspector.
--
-- 'errorMessage', 'failedAccount_errorMessage' - The error message received when the account failed to enable Amazon
-- Inspector.
newFailedAccount ::
  -- | 'accountId'
  Prelude.Text ->
  -- | 'errorCode'
  ErrorCode ->
  -- | 'errorMessage'
  Prelude.Text ->
  FailedAccount
newFailedAccount
  pAccountId_
  pErrorCode_
  pErrorMessage_ =
    FailedAccount'
      { resourceStatus = Prelude.Nothing,
        status = Prelude.Nothing,
        accountId = pAccountId_,
        errorCode = pErrorCode_,
        errorMessage = pErrorMessage_
      }

-- | An object detailing which resources Amazon Inspector is enabled to scan
-- for the account.
failedAccount_resourceStatus :: Lens.Lens' FailedAccount (Prelude.Maybe ResourceStatus)
failedAccount_resourceStatus = Lens.lens (\FailedAccount' {resourceStatus} -> resourceStatus) (\s@FailedAccount' {} a -> s {resourceStatus = a} :: FailedAccount)

-- | The status of Amazon Inspector for the account.
failedAccount_status :: Lens.Lens' FailedAccount (Prelude.Maybe Status)
failedAccount_status = Lens.lens (\FailedAccount' {status} -> status) (\s@FailedAccount' {} a -> s {status = a} :: FailedAccount)

-- | The Amazon Web Services account ID.
failedAccount_accountId :: Lens.Lens' FailedAccount Prelude.Text
failedAccount_accountId = Lens.lens (\FailedAccount' {accountId} -> accountId) (\s@FailedAccount' {} a -> s {accountId = a} :: FailedAccount)

-- | The error code explaining why the account failed to enable Amazon
-- Inspector.
failedAccount_errorCode :: Lens.Lens' FailedAccount ErrorCode
failedAccount_errorCode = Lens.lens (\FailedAccount' {errorCode} -> errorCode) (\s@FailedAccount' {} a -> s {errorCode = a} :: FailedAccount)

-- | The error message received when the account failed to enable Amazon
-- Inspector.
failedAccount_errorMessage :: Lens.Lens' FailedAccount Prelude.Text
failedAccount_errorMessage = Lens.lens (\FailedAccount' {errorMessage} -> errorMessage) (\s@FailedAccount' {} a -> s {errorMessage = a} :: FailedAccount)

instance Data.FromJSON FailedAccount where
  parseJSON =
    Data.withObject
      "FailedAccount"
      ( \x ->
          FailedAccount'
            Prelude.<$> (x Data..:? "resourceStatus")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..: "accountId")
            Prelude.<*> (x Data..: "errorCode")
            Prelude.<*> (x Data..: "errorMessage")
      )

instance Prelude.Hashable FailedAccount where
  hashWithSalt _salt FailedAccount' {..} =
    _salt
      `Prelude.hashWithSalt` resourceStatus
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` errorCode
      `Prelude.hashWithSalt` errorMessage

instance Prelude.NFData FailedAccount where
  rnf FailedAccount' {..} =
    Prelude.rnf resourceStatus
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf errorCode
      `Prelude.seq` Prelude.rnf errorMessage

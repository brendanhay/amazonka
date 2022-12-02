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
-- Module      : Amazonka.MacieV2.Types.UnprocessedAccount
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.UnprocessedAccount where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types.ErrorCode
import qualified Amazonka.Prelude as Prelude

-- | Provides information about an account-related request that hasn\'t been
-- processed.
--
-- /See:/ 'newUnprocessedAccount' smart constructor.
data UnprocessedAccount = UnprocessedAccount'
  { -- | The reason why the request hasn\'t been processed.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services account ID for the account that the request
    -- applies to.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The source of the issue or delay in processing the request.
    errorCode :: Prelude.Maybe ErrorCode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UnprocessedAccount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorMessage', 'unprocessedAccount_errorMessage' - The reason why the request hasn\'t been processed.
--
-- 'accountId', 'unprocessedAccount_accountId' - The Amazon Web Services account ID for the account that the request
-- applies to.
--
-- 'errorCode', 'unprocessedAccount_errorCode' - The source of the issue or delay in processing the request.
newUnprocessedAccount ::
  UnprocessedAccount
newUnprocessedAccount =
  UnprocessedAccount'
    { errorMessage = Prelude.Nothing,
      accountId = Prelude.Nothing,
      errorCode = Prelude.Nothing
    }

-- | The reason why the request hasn\'t been processed.
unprocessedAccount_errorMessage :: Lens.Lens' UnprocessedAccount (Prelude.Maybe Prelude.Text)
unprocessedAccount_errorMessage = Lens.lens (\UnprocessedAccount' {errorMessage} -> errorMessage) (\s@UnprocessedAccount' {} a -> s {errorMessage = a} :: UnprocessedAccount)

-- | The Amazon Web Services account ID for the account that the request
-- applies to.
unprocessedAccount_accountId :: Lens.Lens' UnprocessedAccount (Prelude.Maybe Prelude.Text)
unprocessedAccount_accountId = Lens.lens (\UnprocessedAccount' {accountId} -> accountId) (\s@UnprocessedAccount' {} a -> s {accountId = a} :: UnprocessedAccount)

-- | The source of the issue or delay in processing the request.
unprocessedAccount_errorCode :: Lens.Lens' UnprocessedAccount (Prelude.Maybe ErrorCode)
unprocessedAccount_errorCode = Lens.lens (\UnprocessedAccount' {errorCode} -> errorCode) (\s@UnprocessedAccount' {} a -> s {errorCode = a} :: UnprocessedAccount)

instance Data.FromJSON UnprocessedAccount where
  parseJSON =
    Data.withObject
      "UnprocessedAccount"
      ( \x ->
          UnprocessedAccount'
            Prelude.<$> (x Data..:? "errorMessage")
            Prelude.<*> (x Data..:? "accountId")
            Prelude.<*> (x Data..:? "errorCode")
      )

instance Prelude.Hashable UnprocessedAccount where
  hashWithSalt _salt UnprocessedAccount' {..} =
    _salt `Prelude.hashWithSalt` errorMessage
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` errorCode

instance Prelude.NFData UnprocessedAccount where
  rnf UnprocessedAccount' {..} =
    Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf errorCode

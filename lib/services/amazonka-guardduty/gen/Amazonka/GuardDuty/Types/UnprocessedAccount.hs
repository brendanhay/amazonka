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
-- Module      : Amazonka.GuardDuty.Types.UnprocessedAccount
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.UnprocessedAccount where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the accounts that weren\'t processed.
--
-- /See:/ 'newUnprocessedAccount' smart constructor.
data UnprocessedAccount = UnprocessedAccount'
  { -- | The Amazon Web Services account ID.
    accountId :: Prelude.Text,
    -- | A reason why the account hasn\'t been processed.
    result :: Prelude.Text
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
-- 'accountId', 'unprocessedAccount_accountId' - The Amazon Web Services account ID.
--
-- 'result', 'unprocessedAccount_result' - A reason why the account hasn\'t been processed.
newUnprocessedAccount ::
  -- | 'accountId'
  Prelude.Text ->
  -- | 'result'
  Prelude.Text ->
  UnprocessedAccount
newUnprocessedAccount pAccountId_ pResult_ =
  UnprocessedAccount'
    { accountId = pAccountId_,
      result = pResult_
    }

-- | The Amazon Web Services account ID.
unprocessedAccount_accountId :: Lens.Lens' UnprocessedAccount Prelude.Text
unprocessedAccount_accountId = Lens.lens (\UnprocessedAccount' {accountId} -> accountId) (\s@UnprocessedAccount' {} a -> s {accountId = a} :: UnprocessedAccount)

-- | A reason why the account hasn\'t been processed.
unprocessedAccount_result :: Lens.Lens' UnprocessedAccount Prelude.Text
unprocessedAccount_result = Lens.lens (\UnprocessedAccount' {result} -> result) (\s@UnprocessedAccount' {} a -> s {result = a} :: UnprocessedAccount)

instance Data.FromJSON UnprocessedAccount where
  parseJSON =
    Data.withObject
      "UnprocessedAccount"
      ( \x ->
          UnprocessedAccount'
            Prelude.<$> (x Data..: "accountId")
            Prelude.<*> (x Data..: "result")
      )

instance Prelude.Hashable UnprocessedAccount where
  hashWithSalt _salt UnprocessedAccount' {..} =
    _salt
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` result

instance Prelude.NFData UnprocessedAccount where
  rnf UnprocessedAccount' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf result

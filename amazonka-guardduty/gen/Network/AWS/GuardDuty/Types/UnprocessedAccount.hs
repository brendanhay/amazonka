{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.GuardDuty.Types.UnprocessedAccount
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.UnprocessedAccount where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains information about the accounts that weren\'t processed.
--
-- /See:/ 'newUnprocessedAccount' smart constructor.
data UnprocessedAccount = UnprocessedAccount'
  { -- | The AWS account ID.
    accountId :: Prelude.Text,
    -- | A reason why the account hasn\'t been processed.
    result :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UnprocessedAccount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'unprocessedAccount_accountId' - The AWS account ID.
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

-- | The AWS account ID.
unprocessedAccount_accountId :: Lens.Lens' UnprocessedAccount Prelude.Text
unprocessedAccount_accountId = Lens.lens (\UnprocessedAccount' {accountId} -> accountId) (\s@UnprocessedAccount' {} a -> s {accountId = a} :: UnprocessedAccount)

-- | A reason why the account hasn\'t been processed.
unprocessedAccount_result :: Lens.Lens' UnprocessedAccount Prelude.Text
unprocessedAccount_result = Lens.lens (\UnprocessedAccount' {result} -> result) (\s@UnprocessedAccount' {} a -> s {result = a} :: UnprocessedAccount)

instance Prelude.FromJSON UnprocessedAccount where
  parseJSON =
    Prelude.withObject
      "UnprocessedAccount"
      ( \x ->
          UnprocessedAccount'
            Prelude.<$> (x Prelude..: "accountId")
            Prelude.<*> (x Prelude..: "result")
      )

instance Prelude.Hashable UnprocessedAccount

instance Prelude.NFData UnprocessedAccount

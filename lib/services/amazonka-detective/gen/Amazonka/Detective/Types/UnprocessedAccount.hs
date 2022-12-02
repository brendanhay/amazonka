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
-- Module      : Amazonka.Detective.Types.UnprocessedAccount
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Detective.Types.UnprocessedAccount where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A member account that was included in a request but for which the
-- request could not be processed.
--
-- /See:/ 'newUnprocessedAccount' smart constructor.
data UnprocessedAccount = UnprocessedAccount'
  { -- | The Amazon Web Services account identifier of the member account that
    -- was not processed.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The reason that the member account request could not be processed.
    reason :: Prelude.Maybe Prelude.Text
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
-- 'accountId', 'unprocessedAccount_accountId' - The Amazon Web Services account identifier of the member account that
-- was not processed.
--
-- 'reason', 'unprocessedAccount_reason' - The reason that the member account request could not be processed.
newUnprocessedAccount ::
  UnprocessedAccount
newUnprocessedAccount =
  UnprocessedAccount'
    { accountId = Prelude.Nothing,
      reason = Prelude.Nothing
    }

-- | The Amazon Web Services account identifier of the member account that
-- was not processed.
unprocessedAccount_accountId :: Lens.Lens' UnprocessedAccount (Prelude.Maybe Prelude.Text)
unprocessedAccount_accountId = Lens.lens (\UnprocessedAccount' {accountId} -> accountId) (\s@UnprocessedAccount' {} a -> s {accountId = a} :: UnprocessedAccount)

-- | The reason that the member account request could not be processed.
unprocessedAccount_reason :: Lens.Lens' UnprocessedAccount (Prelude.Maybe Prelude.Text)
unprocessedAccount_reason = Lens.lens (\UnprocessedAccount' {reason} -> reason) (\s@UnprocessedAccount' {} a -> s {reason = a} :: UnprocessedAccount)

instance Data.FromJSON UnprocessedAccount where
  parseJSON =
    Data.withObject
      "UnprocessedAccount"
      ( \x ->
          UnprocessedAccount'
            Prelude.<$> (x Data..:? "AccountId")
            Prelude.<*> (x Data..:? "Reason")
      )

instance Prelude.Hashable UnprocessedAccount where
  hashWithSalt _salt UnprocessedAccount' {..} =
    _salt `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` reason

instance Prelude.NFData UnprocessedAccount where
  rnf UnprocessedAccount' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf reason

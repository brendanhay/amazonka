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
-- Module      : Amazonka.Macie.Types.MemberAccount
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Macie.Types.MemberAccount where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | (Discontinued) Contains information about the Amazon Macie Classic
-- member account.
--
-- /See:/ 'newMemberAccount' smart constructor.
data MemberAccount = MemberAccount'
  { -- | (Discontinued) The Amazon Web Services account ID of the Amazon Macie
    -- Classic member account.
    accountId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MemberAccount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'memberAccount_accountId' - (Discontinued) The Amazon Web Services account ID of the Amazon Macie
-- Classic member account.
newMemberAccount ::
  MemberAccount
newMemberAccount =
  MemberAccount' {accountId = Prelude.Nothing}

-- | (Discontinued) The Amazon Web Services account ID of the Amazon Macie
-- Classic member account.
memberAccount_accountId :: Lens.Lens' MemberAccount (Prelude.Maybe Prelude.Text)
memberAccount_accountId = Lens.lens (\MemberAccount' {accountId} -> accountId) (\s@MemberAccount' {} a -> s {accountId = a} :: MemberAccount)

instance Core.FromJSON MemberAccount where
  parseJSON =
    Core.withObject
      "MemberAccount"
      ( \x ->
          MemberAccount' Prelude.<$> (x Core..:? "accountId")
      )

instance Prelude.Hashable MemberAccount where
  hashWithSalt _salt MemberAccount' {..} =
    _salt `Prelude.hashWithSalt` accountId

instance Prelude.NFData MemberAccount where
  rnf MemberAccount' {..} = Prelude.rnf accountId

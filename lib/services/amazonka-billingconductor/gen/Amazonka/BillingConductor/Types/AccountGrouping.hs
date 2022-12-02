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
-- Module      : Amazonka.BillingConductor.Types.AccountGrouping
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.BillingConductor.Types.AccountGrouping where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The set of accounts that will be under the billing group. The set of
-- accounts resemble the linked accounts in a consolidated family.
--
-- /See:/ 'newAccountGrouping' smart constructor.
data AccountGrouping = AccountGrouping'
  { -- | The account IDs that make up the billing group. Account IDs must be a
    -- part of the consolidated billing family, and not associated with another
    -- billing group.
    linkedAccountIds :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AccountGrouping' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'linkedAccountIds', 'accountGrouping_linkedAccountIds' - The account IDs that make up the billing group. Account IDs must be a
-- part of the consolidated billing family, and not associated with another
-- billing group.
newAccountGrouping ::
  -- | 'linkedAccountIds'
  Prelude.NonEmpty Prelude.Text ->
  AccountGrouping
newAccountGrouping pLinkedAccountIds_ =
  AccountGrouping'
    { linkedAccountIds =
        Lens.coerced Lens.# pLinkedAccountIds_
    }

-- | The account IDs that make up the billing group. Account IDs must be a
-- part of the consolidated billing family, and not associated with another
-- billing group.
accountGrouping_linkedAccountIds :: Lens.Lens' AccountGrouping (Prelude.NonEmpty Prelude.Text)
accountGrouping_linkedAccountIds = Lens.lens (\AccountGrouping' {linkedAccountIds} -> linkedAccountIds) (\s@AccountGrouping' {} a -> s {linkedAccountIds = a} :: AccountGrouping) Prelude.. Lens.coerced

instance Prelude.Hashable AccountGrouping where
  hashWithSalt _salt AccountGrouping' {..} =
    _salt `Prelude.hashWithSalt` linkedAccountIds

instance Prelude.NFData AccountGrouping where
  rnf AccountGrouping' {..} =
    Prelude.rnf linkedAccountIds

instance Data.ToJSON AccountGrouping where
  toJSON AccountGrouping' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("LinkedAccountIds" Data..= linkedAccountIds)
          ]
      )

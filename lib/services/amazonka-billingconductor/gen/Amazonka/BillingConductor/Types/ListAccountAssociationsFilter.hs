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
-- Module      : Amazonka.BillingConductor.Types.ListAccountAssociationsFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.BillingConductor.Types.ListAccountAssociationsFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The filter on the account ID of the linked account, or any of the
-- following:
--
-- @MONITORED@: linked accounts that are associated to billing groups.
--
-- @UNMONITORED@: linked accounts that are not associated to billing
-- groups.
--
-- @Billing Group Arn@: linked accounts that are associated to the provided
-- Billing Group Arn.
--
-- /See:/ 'newListAccountAssociationsFilter' smart constructor.
data ListAccountAssociationsFilter = ListAccountAssociationsFilter'
  { -- | The Amazon Web Services account ID to filter on.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The list of Amazon Web Services IDs to retrieve their associated billing
    -- group for a given time range.
    accountIds :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | @MONITORED@: linked accounts that are associated to billing groups.
    --
    -- @UNMONITORED@: linked accounts that are not associated to billing
    -- groups.
    --
    -- @Billing Group Arn@: linked accounts that are associated to the provided
    -- Billing Group Arn.
    association :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAccountAssociationsFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'listAccountAssociationsFilter_accountId' - The Amazon Web Services account ID to filter on.
--
-- 'accountIds', 'listAccountAssociationsFilter_accountIds' - The list of Amazon Web Services IDs to retrieve their associated billing
-- group for a given time range.
--
-- 'association', 'listAccountAssociationsFilter_association' - @MONITORED@: linked accounts that are associated to billing groups.
--
-- @UNMONITORED@: linked accounts that are not associated to billing
-- groups.
--
-- @Billing Group Arn@: linked accounts that are associated to the provided
-- Billing Group Arn.
newListAccountAssociationsFilter ::
  ListAccountAssociationsFilter
newListAccountAssociationsFilter =
  ListAccountAssociationsFilter'
    { accountId =
        Prelude.Nothing,
      accountIds = Prelude.Nothing,
      association = Prelude.Nothing
    }

-- | The Amazon Web Services account ID to filter on.
listAccountAssociationsFilter_accountId :: Lens.Lens' ListAccountAssociationsFilter (Prelude.Maybe Prelude.Text)
listAccountAssociationsFilter_accountId = Lens.lens (\ListAccountAssociationsFilter' {accountId} -> accountId) (\s@ListAccountAssociationsFilter' {} a -> s {accountId = a} :: ListAccountAssociationsFilter)

-- | The list of Amazon Web Services IDs to retrieve their associated billing
-- group for a given time range.
listAccountAssociationsFilter_accountIds :: Lens.Lens' ListAccountAssociationsFilter (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
listAccountAssociationsFilter_accountIds = Lens.lens (\ListAccountAssociationsFilter' {accountIds} -> accountIds) (\s@ListAccountAssociationsFilter' {} a -> s {accountIds = a} :: ListAccountAssociationsFilter) Prelude.. Lens.mapping Lens.coerced

-- | @MONITORED@: linked accounts that are associated to billing groups.
--
-- @UNMONITORED@: linked accounts that are not associated to billing
-- groups.
--
-- @Billing Group Arn@: linked accounts that are associated to the provided
-- Billing Group Arn.
listAccountAssociationsFilter_association :: Lens.Lens' ListAccountAssociationsFilter (Prelude.Maybe Prelude.Text)
listAccountAssociationsFilter_association = Lens.lens (\ListAccountAssociationsFilter' {association} -> association) (\s@ListAccountAssociationsFilter' {} a -> s {association = a} :: ListAccountAssociationsFilter)

instance
  Prelude.Hashable
    ListAccountAssociationsFilter
  where
  hashWithSalt _salt ListAccountAssociationsFilter' {..} =
    _salt
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` accountIds
      `Prelude.hashWithSalt` association

instance Prelude.NFData ListAccountAssociationsFilter where
  rnf ListAccountAssociationsFilter' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf accountIds
      `Prelude.seq` Prelude.rnf association

instance Data.ToJSON ListAccountAssociationsFilter where
  toJSON ListAccountAssociationsFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AccountId" Data..=) Prelude.<$> accountId,
            ("AccountIds" Data..=) Prelude.<$> accountIds,
            ("Association" Data..=) Prelude.<$> association
          ]
      )

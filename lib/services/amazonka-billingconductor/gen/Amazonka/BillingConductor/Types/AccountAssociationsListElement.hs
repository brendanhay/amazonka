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
-- Module      : Amazonka.BillingConductor.Types.AccountAssociationsListElement
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.BillingConductor.Types.AccountAssociationsListElement where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A representation of a linked account.
--
-- /See:/ 'newAccountAssociationsListElement' smart constructor.
data AccountAssociationsListElement = AccountAssociationsListElement'
  { -- | The Billing Group Arn that the linked account is associated to.
    billingGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The associating array of account IDs.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services account name.
    accountName :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The Amazon Web Services account email.
    accountEmail :: Prelude.Maybe (Core.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AccountAssociationsListElement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'billingGroupArn', 'accountAssociationsListElement_billingGroupArn' - The Billing Group Arn that the linked account is associated to.
--
-- 'accountId', 'accountAssociationsListElement_accountId' - The associating array of account IDs.
--
-- 'accountName', 'accountAssociationsListElement_accountName' - The Amazon Web Services account name.
--
-- 'accountEmail', 'accountAssociationsListElement_accountEmail' - The Amazon Web Services account email.
newAccountAssociationsListElement ::
  AccountAssociationsListElement
newAccountAssociationsListElement =
  AccountAssociationsListElement'
    { billingGroupArn =
        Prelude.Nothing,
      accountId = Prelude.Nothing,
      accountName = Prelude.Nothing,
      accountEmail = Prelude.Nothing
    }

-- | The Billing Group Arn that the linked account is associated to.
accountAssociationsListElement_billingGroupArn :: Lens.Lens' AccountAssociationsListElement (Prelude.Maybe Prelude.Text)
accountAssociationsListElement_billingGroupArn = Lens.lens (\AccountAssociationsListElement' {billingGroupArn} -> billingGroupArn) (\s@AccountAssociationsListElement' {} a -> s {billingGroupArn = a} :: AccountAssociationsListElement)

-- | The associating array of account IDs.
accountAssociationsListElement_accountId :: Lens.Lens' AccountAssociationsListElement (Prelude.Maybe Prelude.Text)
accountAssociationsListElement_accountId = Lens.lens (\AccountAssociationsListElement' {accountId} -> accountId) (\s@AccountAssociationsListElement' {} a -> s {accountId = a} :: AccountAssociationsListElement)

-- | The Amazon Web Services account name.
accountAssociationsListElement_accountName :: Lens.Lens' AccountAssociationsListElement (Prelude.Maybe Prelude.Text)
accountAssociationsListElement_accountName = Lens.lens (\AccountAssociationsListElement' {accountName} -> accountName) (\s@AccountAssociationsListElement' {} a -> s {accountName = a} :: AccountAssociationsListElement) Prelude.. Lens.mapping Core._Sensitive

-- | The Amazon Web Services account email.
accountAssociationsListElement_accountEmail :: Lens.Lens' AccountAssociationsListElement (Prelude.Maybe Prelude.Text)
accountAssociationsListElement_accountEmail = Lens.lens (\AccountAssociationsListElement' {accountEmail} -> accountEmail) (\s@AccountAssociationsListElement' {} a -> s {accountEmail = a} :: AccountAssociationsListElement) Prelude.. Lens.mapping Core._Sensitive

instance Core.FromJSON AccountAssociationsListElement where
  parseJSON =
    Core.withObject
      "AccountAssociationsListElement"
      ( \x ->
          AccountAssociationsListElement'
            Prelude.<$> (x Core..:? "BillingGroupArn")
            Prelude.<*> (x Core..:? "AccountId")
            Prelude.<*> (x Core..:? "AccountName")
            Prelude.<*> (x Core..:? "AccountEmail")
      )

instance
  Prelude.Hashable
    AccountAssociationsListElement
  where
  hashWithSalt
    _salt
    AccountAssociationsListElement' {..} =
      _salt `Prelude.hashWithSalt` billingGroupArn
        `Prelude.hashWithSalt` accountId
        `Prelude.hashWithSalt` accountName
        `Prelude.hashWithSalt` accountEmail

instance
  Prelude.NFData
    AccountAssociationsListElement
  where
  rnf AccountAssociationsListElement' {..} =
    Prelude.rnf billingGroupArn
      `Prelude.seq` Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf accountName
      `Prelude.seq` Prelude.rnf accountEmail

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
-- Module      : Amazonka.Inspector2.Types.AccountState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.AccountState where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector2.Types.ResourceState
import Amazonka.Inspector2.Types.State
import qualified Amazonka.Prelude as Prelude

-- | An object with details the status of an Amazon Web Services account
-- within your Amazon Inspector environment.
--
-- /See:/ 'newAccountState' smart constructor.
data AccountState = AccountState'
  { -- | The Amazon Web Services account ID.
    accountId :: Prelude.Text,
    -- | An object detailing which resources Amazon Inspector is enabled to scan
    -- for the account.
    resourceState :: ResourceState,
    -- | An object detailing the status of Amazon Inspector for the account.
    state :: State
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AccountState' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'accountState_accountId' - The Amazon Web Services account ID.
--
-- 'resourceState', 'accountState_resourceState' - An object detailing which resources Amazon Inspector is enabled to scan
-- for the account.
--
-- 'state', 'accountState_state' - An object detailing the status of Amazon Inspector for the account.
newAccountState ::
  -- | 'accountId'
  Prelude.Text ->
  -- | 'resourceState'
  ResourceState ->
  -- | 'state'
  State ->
  AccountState
newAccountState pAccountId_ pResourceState_ pState_ =
  AccountState'
    { accountId = pAccountId_,
      resourceState = pResourceState_,
      state = pState_
    }

-- | The Amazon Web Services account ID.
accountState_accountId :: Lens.Lens' AccountState Prelude.Text
accountState_accountId = Lens.lens (\AccountState' {accountId} -> accountId) (\s@AccountState' {} a -> s {accountId = a} :: AccountState)

-- | An object detailing which resources Amazon Inspector is enabled to scan
-- for the account.
accountState_resourceState :: Lens.Lens' AccountState ResourceState
accountState_resourceState = Lens.lens (\AccountState' {resourceState} -> resourceState) (\s@AccountState' {} a -> s {resourceState = a} :: AccountState)

-- | An object detailing the status of Amazon Inspector for the account.
accountState_state :: Lens.Lens' AccountState State
accountState_state = Lens.lens (\AccountState' {state} -> state) (\s@AccountState' {} a -> s {state = a} :: AccountState)

instance Data.FromJSON AccountState where
  parseJSON =
    Data.withObject
      "AccountState"
      ( \x ->
          AccountState'
            Prelude.<$> (x Data..: "accountId")
            Prelude.<*> (x Data..: "resourceState")
            Prelude.<*> (x Data..: "state")
      )

instance Prelude.Hashable AccountState where
  hashWithSalt _salt AccountState' {..} =
    _salt `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` resourceState
      `Prelude.hashWithSalt` state

instance Prelude.NFData AccountState where
  rnf AccountState' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf resourceState
      `Prelude.seq` Prelude.rnf state

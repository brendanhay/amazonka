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
-- Module      : Amazonka.CognitoIdentityProvider.Types.AccountTakeoverActionType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoIdentityProvider.Types.AccountTakeoverActionType where

import Amazonka.CognitoIdentityProvider.Types.AccountTakeoverEventActionType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Account takeover action type.
--
-- /See:/ 'newAccountTakeoverActionType' smart constructor.
data AccountTakeoverActionType = AccountTakeoverActionType'
  { -- | Flag specifying whether to send a notification.
    notify :: Prelude.Bool,
    -- | The action to take in response to the account takeover action. Valid
    -- values are as follows:
    --
    -- -   @BLOCK@ Choosing this action will block the request.
    --
    -- -   @MFA_IF_CONFIGURED@ Present an MFA challenge if user has configured
    --     it, else allow the request.
    --
    -- -   @MFA_REQUIRED@ Present an MFA challenge if user has configured it,
    --     else block the request.
    --
    -- -   @NO_ACTION@ Allow the user to sign in.
    eventAction :: AccountTakeoverEventActionType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AccountTakeoverActionType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'notify', 'accountTakeoverActionType_notify' - Flag specifying whether to send a notification.
--
-- 'eventAction', 'accountTakeoverActionType_eventAction' - The action to take in response to the account takeover action. Valid
-- values are as follows:
--
-- -   @BLOCK@ Choosing this action will block the request.
--
-- -   @MFA_IF_CONFIGURED@ Present an MFA challenge if user has configured
--     it, else allow the request.
--
-- -   @MFA_REQUIRED@ Present an MFA challenge if user has configured it,
--     else block the request.
--
-- -   @NO_ACTION@ Allow the user to sign in.
newAccountTakeoverActionType ::
  -- | 'notify'
  Prelude.Bool ->
  -- | 'eventAction'
  AccountTakeoverEventActionType ->
  AccountTakeoverActionType
newAccountTakeoverActionType pNotify_ pEventAction_ =
  AccountTakeoverActionType'
    { notify = pNotify_,
      eventAction = pEventAction_
    }

-- | Flag specifying whether to send a notification.
accountTakeoverActionType_notify :: Lens.Lens' AccountTakeoverActionType Prelude.Bool
accountTakeoverActionType_notify = Lens.lens (\AccountTakeoverActionType' {notify} -> notify) (\s@AccountTakeoverActionType' {} a -> s {notify = a} :: AccountTakeoverActionType)

-- | The action to take in response to the account takeover action. Valid
-- values are as follows:
--
-- -   @BLOCK@ Choosing this action will block the request.
--
-- -   @MFA_IF_CONFIGURED@ Present an MFA challenge if user has configured
--     it, else allow the request.
--
-- -   @MFA_REQUIRED@ Present an MFA challenge if user has configured it,
--     else block the request.
--
-- -   @NO_ACTION@ Allow the user to sign in.
accountTakeoverActionType_eventAction :: Lens.Lens' AccountTakeoverActionType AccountTakeoverEventActionType
accountTakeoverActionType_eventAction = Lens.lens (\AccountTakeoverActionType' {eventAction} -> eventAction) (\s@AccountTakeoverActionType' {} a -> s {eventAction = a} :: AccountTakeoverActionType)

instance Data.FromJSON AccountTakeoverActionType where
  parseJSON =
    Data.withObject
      "AccountTakeoverActionType"
      ( \x ->
          AccountTakeoverActionType'
            Prelude.<$> (x Data..: "Notify")
            Prelude.<*> (x Data..: "EventAction")
      )

instance Prelude.Hashable AccountTakeoverActionType where
  hashWithSalt _salt AccountTakeoverActionType' {..} =
    _salt
      `Prelude.hashWithSalt` notify
      `Prelude.hashWithSalt` eventAction

instance Prelude.NFData AccountTakeoverActionType where
  rnf AccountTakeoverActionType' {..} =
    Prelude.rnf notify
      `Prelude.seq` Prelude.rnf eventAction

instance Data.ToJSON AccountTakeoverActionType where
  toJSON AccountTakeoverActionType' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Notify" Data..= notify),
            Prelude.Just ("EventAction" Data..= eventAction)
          ]
      )

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
-- Module      : Network.AWS.CognitoIdentityProvider.Types.AccountTakeoverActionType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.AccountTakeoverActionType where

import Network.AWS.CognitoIdentityProvider.Types.AccountTakeoverEventActionType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Account takeover action type.
--
-- /See:/ 'newAccountTakeoverActionType' smart constructor.
data AccountTakeoverActionType = AccountTakeoverActionType'
  { -- | Flag specifying whether to send a notification.
    notify :: Prelude.Bool,
    -- | The event action.
    --
    -- -   @BLOCK@ Choosing this action will block the request.
    --
    -- -   @MFA_IF_CONFIGURED@ Throw MFA challenge if user has configured it,
    --     else allow the request.
    --
    -- -   @MFA_REQUIRED@ Throw MFA challenge if user has configured it, else
    --     block the request.
    --
    -- -   @NO_ACTION@ Allow the user sign-in.
    eventAction :: AccountTakeoverEventActionType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'eventAction', 'accountTakeoverActionType_eventAction' - The event action.
--
-- -   @BLOCK@ Choosing this action will block the request.
--
-- -   @MFA_IF_CONFIGURED@ Throw MFA challenge if user has configured it,
--     else allow the request.
--
-- -   @MFA_REQUIRED@ Throw MFA challenge if user has configured it, else
--     block the request.
--
-- -   @NO_ACTION@ Allow the user sign-in.
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

-- | The event action.
--
-- -   @BLOCK@ Choosing this action will block the request.
--
-- -   @MFA_IF_CONFIGURED@ Throw MFA challenge if user has configured it,
--     else allow the request.
--
-- -   @MFA_REQUIRED@ Throw MFA challenge if user has configured it, else
--     block the request.
--
-- -   @NO_ACTION@ Allow the user sign-in.
accountTakeoverActionType_eventAction :: Lens.Lens' AccountTakeoverActionType AccountTakeoverEventActionType
accountTakeoverActionType_eventAction = Lens.lens (\AccountTakeoverActionType' {eventAction} -> eventAction) (\s@AccountTakeoverActionType' {} a -> s {eventAction = a} :: AccountTakeoverActionType)

instance Prelude.FromJSON AccountTakeoverActionType where
  parseJSON =
    Prelude.withObject
      "AccountTakeoverActionType"
      ( \x ->
          AccountTakeoverActionType'
            Prelude.<$> (x Prelude..: "Notify")
            Prelude.<*> (x Prelude..: "EventAction")
      )

instance Prelude.Hashable AccountTakeoverActionType

instance Prelude.NFData AccountTakeoverActionType

instance Prelude.ToJSON AccountTakeoverActionType where
  toJSON AccountTakeoverActionType' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Notify" Prelude..= notify),
            Prelude.Just ("EventAction" Prelude..= eventAction)
          ]
      )

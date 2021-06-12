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
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Account takeover action type.
--
-- /See:/ 'newAccountTakeoverActionType' smart constructor.
data AccountTakeoverActionType = AccountTakeoverActionType'
  { -- | Flag specifying whether to send a notification.
    notify :: Core.Bool,
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
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Bool ->
  -- | 'eventAction'
  AccountTakeoverEventActionType ->
  AccountTakeoverActionType
newAccountTakeoverActionType pNotify_ pEventAction_ =
  AccountTakeoverActionType'
    { notify = pNotify_,
      eventAction = pEventAction_
    }

-- | Flag specifying whether to send a notification.
accountTakeoverActionType_notify :: Lens.Lens' AccountTakeoverActionType Core.Bool
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

instance Core.FromJSON AccountTakeoverActionType where
  parseJSON =
    Core.withObject
      "AccountTakeoverActionType"
      ( \x ->
          AccountTakeoverActionType'
            Core.<$> (x Core..: "Notify")
            Core.<*> (x Core..: "EventAction")
      )

instance Core.Hashable AccountTakeoverActionType

instance Core.NFData AccountTakeoverActionType

instance Core.ToJSON AccountTakeoverActionType where
  toJSON AccountTakeoverActionType' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Notify" Core..= notify),
            Core.Just ("EventAction" Core..= eventAction)
          ]
      )

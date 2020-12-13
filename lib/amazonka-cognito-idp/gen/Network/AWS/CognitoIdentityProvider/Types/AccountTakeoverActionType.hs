{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.AccountTakeoverActionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.AccountTakeoverActionType
  ( AccountTakeoverActionType (..),

    -- * Smart constructor
    mkAccountTakeoverActionType,

    -- * Lenses
    atatEventAction,
    atatNotify,
  )
where

import Network.AWS.CognitoIdentityProvider.Types.AccountTakeoverEventActionType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Account takeover action type.
--
-- /See:/ 'mkAccountTakeoverActionType' smart constructor.
data AccountTakeoverActionType = AccountTakeoverActionType'
  { -- | The event action.
    --
    --
    --     * @BLOCK@ Choosing this action will block the request.
    --
    --
    --     * @MFA_IF_CONFIGURED@ Throw MFA challenge if user has configured it, else allow the request.
    --
    --
    --     * @MFA_REQUIRED@ Throw MFA challenge if user has configured it, else block the request.
    --
    --
    --     * @NO_ACTION@ Allow the user sign-in.
    eventAction :: AccountTakeoverEventActionType,
    -- | Flag specifying whether to send a notification.
    notify :: Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AccountTakeoverActionType' with the minimum fields required to make a request.
--
-- * 'eventAction' - The event action.
--
--
--     * @BLOCK@ Choosing this action will block the request.
--
--
--     * @MFA_IF_CONFIGURED@ Throw MFA challenge if user has configured it, else allow the request.
--
--
--     * @MFA_REQUIRED@ Throw MFA challenge if user has configured it, else block the request.
--
--
--     * @NO_ACTION@ Allow the user sign-in.
--
--
-- * 'notify' - Flag specifying whether to send a notification.
mkAccountTakeoverActionType ::
  -- | 'eventAction'
  AccountTakeoverEventActionType ->
  -- | 'notify'
  Lude.Bool ->
  AccountTakeoverActionType
mkAccountTakeoverActionType pEventAction_ pNotify_ =
  AccountTakeoverActionType'
    { eventAction = pEventAction_,
      notify = pNotify_
    }

-- | The event action.
--
--
--     * @BLOCK@ Choosing this action will block the request.
--
--
--     * @MFA_IF_CONFIGURED@ Throw MFA challenge if user has configured it, else allow the request.
--
--
--     * @MFA_REQUIRED@ Throw MFA challenge if user has configured it, else block the request.
--
--
--     * @NO_ACTION@ Allow the user sign-in.
--
--
--
-- /Note:/ Consider using 'eventAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atatEventAction :: Lens.Lens' AccountTakeoverActionType AccountTakeoverEventActionType
atatEventAction = Lens.lens (eventAction :: AccountTakeoverActionType -> AccountTakeoverEventActionType) (\s a -> s {eventAction = a} :: AccountTakeoverActionType)
{-# DEPRECATED atatEventAction "Use generic-lens or generic-optics with 'eventAction' instead." #-}

-- | Flag specifying whether to send a notification.
--
-- /Note:/ Consider using 'notify' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atatNotify :: Lens.Lens' AccountTakeoverActionType Lude.Bool
atatNotify = Lens.lens (notify :: AccountTakeoverActionType -> Lude.Bool) (\s a -> s {notify = a} :: AccountTakeoverActionType)
{-# DEPRECATED atatNotify "Use generic-lens or generic-optics with 'notify' instead." #-}

instance Lude.FromJSON AccountTakeoverActionType where
  parseJSON =
    Lude.withObject
      "AccountTakeoverActionType"
      ( \x ->
          AccountTakeoverActionType'
            Lude.<$> (x Lude..: "EventAction") Lude.<*> (x Lude..: "Notify")
      )

instance Lude.ToJSON AccountTakeoverActionType where
  toJSON AccountTakeoverActionType' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("EventAction" Lude..= eventAction),
            Lude.Just ("Notify" Lude..= notify)
          ]
      )

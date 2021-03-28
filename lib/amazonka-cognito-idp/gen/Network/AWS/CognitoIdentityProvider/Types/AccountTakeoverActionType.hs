{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.AccountTakeoverActionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoIdentityProvider.Types.AccountTakeoverActionType
  ( AccountTakeoverActionType (..)
  -- * Smart constructor
  , mkAccountTakeoverActionType
  -- * Lenses
  , atatNotify
  , atatEventAction
  ) where

import qualified Network.AWS.CognitoIdentityProvider.Types.AccountTakeoverEventActionType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Account takeover action type.
--
-- /See:/ 'mkAccountTakeoverActionType' smart constructor.
data AccountTakeoverActionType = AccountTakeoverActionType'
  { notify :: Core.Bool
    -- ^ Flag specifying whether to send a notification.
  , eventAction :: Types.AccountTakeoverEventActionType
    -- ^ The event action.
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
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AccountTakeoverActionType' value with any optional fields omitted.
mkAccountTakeoverActionType
    :: Core.Bool -- ^ 'notify'
    -> Types.AccountTakeoverEventActionType -- ^ 'eventAction'
    -> AccountTakeoverActionType
mkAccountTakeoverActionType notify eventAction
  = AccountTakeoverActionType'{notify, eventAction}

-- | Flag specifying whether to send a notification.
--
-- /Note:/ Consider using 'notify' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atatNotify :: Lens.Lens' AccountTakeoverActionType Core.Bool
atatNotify = Lens.field @"notify"
{-# INLINEABLE atatNotify #-}
{-# DEPRECATED notify "Use generic-lens or generic-optics with 'notify' instead"  #-}

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
atatEventAction :: Lens.Lens' AccountTakeoverActionType Types.AccountTakeoverEventActionType
atatEventAction = Lens.field @"eventAction"
{-# INLINEABLE atatEventAction #-}
{-# DEPRECATED eventAction "Use generic-lens or generic-optics with 'eventAction' instead"  #-}

instance Core.FromJSON AccountTakeoverActionType where
        toJSON AccountTakeoverActionType{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Notify" Core..= notify),
                  Core.Just ("EventAction" Core..= eventAction)])

instance Core.FromJSON AccountTakeoverActionType where
        parseJSON
          = Core.withObject "AccountTakeoverActionType" Core.$
              \ x ->
                AccountTakeoverActionType' Core.<$>
                  (x Core..: "Notify") Core.<*> x Core..: "EventAction"

{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.AccountTakeoverRiskConfigurationType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoIdentityProvider.Types.AccountTakeoverRiskConfigurationType
  ( AccountTakeoverRiskConfigurationType (..)
  -- * Smart constructor
  , mkAccountTakeoverRiskConfigurationType
  -- * Lenses
  , atrctActions
  , atrctNotifyConfiguration
  ) where

import qualified Network.AWS.CognitoIdentityProvider.Types.AccountTakeoverActionsType as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.NotifyConfigurationType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Configuration for mitigation actions and notification for different levels of risk detected for a potential account takeover.
--
-- /See:/ 'mkAccountTakeoverRiskConfigurationType' smart constructor.
data AccountTakeoverRiskConfigurationType = AccountTakeoverRiskConfigurationType'
  { actions :: Types.AccountTakeoverActionsType
    -- ^ Account takeover risk configuration actions
  , notifyConfiguration :: Core.Maybe Types.NotifyConfigurationType
    -- ^ The notify configuration used to construct email notifications.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AccountTakeoverRiskConfigurationType' value with any optional fields omitted.
mkAccountTakeoverRiskConfigurationType
    :: Types.AccountTakeoverActionsType -- ^ 'actions'
    -> AccountTakeoverRiskConfigurationType
mkAccountTakeoverRiskConfigurationType actions
  = AccountTakeoverRiskConfigurationType'{actions,
                                          notifyConfiguration = Core.Nothing}

-- | Account takeover risk configuration actions
--
-- /Note:/ Consider using 'actions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atrctActions :: Lens.Lens' AccountTakeoverRiskConfigurationType Types.AccountTakeoverActionsType
atrctActions = Lens.field @"actions"
{-# INLINEABLE atrctActions #-}
{-# DEPRECATED actions "Use generic-lens or generic-optics with 'actions' instead"  #-}

-- | The notify configuration used to construct email notifications.
--
-- /Note:/ Consider using 'notifyConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atrctNotifyConfiguration :: Lens.Lens' AccountTakeoverRiskConfigurationType (Core.Maybe Types.NotifyConfigurationType)
atrctNotifyConfiguration = Lens.field @"notifyConfiguration"
{-# INLINEABLE atrctNotifyConfiguration #-}
{-# DEPRECATED notifyConfiguration "Use generic-lens or generic-optics with 'notifyConfiguration' instead"  #-}

instance Core.FromJSON AccountTakeoverRiskConfigurationType where
        toJSON AccountTakeoverRiskConfigurationType{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Actions" Core..= actions),
                  ("NotifyConfiguration" Core..=) Core.<$> notifyConfiguration])

instance Core.FromJSON AccountTakeoverRiskConfigurationType where
        parseJSON
          = Core.withObject "AccountTakeoverRiskConfigurationType" Core.$
              \ x ->
                AccountTakeoverRiskConfigurationType' Core.<$>
                  (x Core..: "Actions") Core.<*> x Core..:? "NotifyConfiguration"

{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.AccountTakeoverActionsType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoIdentityProvider.Types.AccountTakeoverActionsType
  ( AccountTakeoverActionsType (..)
  -- * Smart constructor
  , mkAccountTakeoverActionsType
  -- * Lenses
  , atatHighAction
  , atatLowAction
  , atatMediumAction
  ) where

import qualified Network.AWS.CognitoIdentityProvider.Types.AccountTakeoverActionType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Account takeover actions type.
--
-- /See:/ 'mkAccountTakeoverActionsType' smart constructor.
data AccountTakeoverActionsType = AccountTakeoverActionsType'
  { highAction :: Core.Maybe Types.AccountTakeoverActionType
    -- ^ Action to take for a high risk.
  , lowAction :: Core.Maybe Types.AccountTakeoverActionType
    -- ^ Action to take for a low risk.
  , mediumAction :: Core.Maybe Types.AccountTakeoverActionType
    -- ^ Action to take for a medium risk.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AccountTakeoverActionsType' value with any optional fields omitted.
mkAccountTakeoverActionsType
    :: AccountTakeoverActionsType
mkAccountTakeoverActionsType
  = AccountTakeoverActionsType'{highAction = Core.Nothing,
                                lowAction = Core.Nothing, mediumAction = Core.Nothing}

-- | Action to take for a high risk.
--
-- /Note:/ Consider using 'highAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atatHighAction :: Lens.Lens' AccountTakeoverActionsType (Core.Maybe Types.AccountTakeoverActionType)
atatHighAction = Lens.field @"highAction"
{-# INLINEABLE atatHighAction #-}
{-# DEPRECATED highAction "Use generic-lens or generic-optics with 'highAction' instead"  #-}

-- | Action to take for a low risk.
--
-- /Note:/ Consider using 'lowAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atatLowAction :: Lens.Lens' AccountTakeoverActionsType (Core.Maybe Types.AccountTakeoverActionType)
atatLowAction = Lens.field @"lowAction"
{-# INLINEABLE atatLowAction #-}
{-# DEPRECATED lowAction "Use generic-lens or generic-optics with 'lowAction' instead"  #-}

-- | Action to take for a medium risk.
--
-- /Note:/ Consider using 'mediumAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atatMediumAction :: Lens.Lens' AccountTakeoverActionsType (Core.Maybe Types.AccountTakeoverActionType)
atatMediumAction = Lens.field @"mediumAction"
{-# INLINEABLE atatMediumAction #-}
{-# DEPRECATED mediumAction "Use generic-lens or generic-optics with 'mediumAction' instead"  #-}

instance Core.FromJSON AccountTakeoverActionsType where
        toJSON AccountTakeoverActionsType{..}
          = Core.object
              (Core.catMaybes
                 [("HighAction" Core..=) Core.<$> highAction,
                  ("LowAction" Core..=) Core.<$> lowAction,
                  ("MediumAction" Core..=) Core.<$> mediumAction])

instance Core.FromJSON AccountTakeoverActionsType where
        parseJSON
          = Core.withObject "AccountTakeoverActionsType" Core.$
              \ x ->
                AccountTakeoverActionsType' Core.<$>
                  (x Core..:? "HighAction") Core.<*> x Core..:? "LowAction" Core.<*>
                    x Core..:? "MediumAction"

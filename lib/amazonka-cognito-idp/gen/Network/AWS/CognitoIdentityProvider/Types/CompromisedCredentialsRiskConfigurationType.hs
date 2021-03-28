{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.CompromisedCredentialsRiskConfigurationType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoIdentityProvider.Types.CompromisedCredentialsRiskConfigurationType
  ( CompromisedCredentialsRiskConfigurationType (..)
  -- * Smart constructor
  , mkCompromisedCredentialsRiskConfigurationType
  -- * Lenses
  , ccrctActions
  , ccrctEventFilter
  ) where

import qualified Network.AWS.CognitoIdentityProvider.Types.CompromisedCredentialsActionsType as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.EventFilterType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The compromised credentials risk configuration type.
--
-- /See:/ 'mkCompromisedCredentialsRiskConfigurationType' smart constructor.
data CompromisedCredentialsRiskConfigurationType = CompromisedCredentialsRiskConfigurationType'
  { actions :: Types.CompromisedCredentialsActionsType
    -- ^ The compromised credentials risk configuration actions.
  , eventFilter :: Core.Maybe [Types.EventFilterType]
    -- ^ Perform the action for these events. The default is to perform all events if no event filter is specified.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CompromisedCredentialsRiskConfigurationType' value with any optional fields omitted.
mkCompromisedCredentialsRiskConfigurationType
    :: Types.CompromisedCredentialsActionsType -- ^ 'actions'
    -> CompromisedCredentialsRiskConfigurationType
mkCompromisedCredentialsRiskConfigurationType actions
  = CompromisedCredentialsRiskConfigurationType'{actions,
                                                 eventFilter = Core.Nothing}

-- | The compromised credentials risk configuration actions.
--
-- /Note:/ Consider using 'actions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrctActions :: Lens.Lens' CompromisedCredentialsRiskConfigurationType Types.CompromisedCredentialsActionsType
ccrctActions = Lens.field @"actions"
{-# INLINEABLE ccrctActions #-}
{-# DEPRECATED actions "Use generic-lens or generic-optics with 'actions' instead"  #-}

-- | Perform the action for these events. The default is to perform all events if no event filter is specified.
--
-- /Note:/ Consider using 'eventFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrctEventFilter :: Lens.Lens' CompromisedCredentialsRiskConfigurationType (Core.Maybe [Types.EventFilterType])
ccrctEventFilter = Lens.field @"eventFilter"
{-# INLINEABLE ccrctEventFilter #-}
{-# DEPRECATED eventFilter "Use generic-lens or generic-optics with 'eventFilter' instead"  #-}

instance Core.FromJSON CompromisedCredentialsRiskConfigurationType
         where
        toJSON CompromisedCredentialsRiskConfigurationType{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Actions" Core..= actions),
                  ("EventFilter" Core..=) Core.<$> eventFilter])

instance Core.FromJSON CompromisedCredentialsRiskConfigurationType
         where
        parseJSON
          = Core.withObject "CompromisedCredentialsRiskConfigurationType"
              Core.$
              \ x ->
                CompromisedCredentialsRiskConfigurationType' Core.<$>
                  (x Core..: "Actions") Core.<*> x Core..:? "EventFilter"

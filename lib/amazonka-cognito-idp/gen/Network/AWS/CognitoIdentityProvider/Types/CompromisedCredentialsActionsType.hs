{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.CompromisedCredentialsActionsType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.CompromisedCredentialsActionsType
  ( CompromisedCredentialsActionsType (..),

    -- * Smart constructor
    mkCompromisedCredentialsActionsType,

    -- * Lenses
    ccatEventAction,
  )
where

import qualified Network.AWS.CognitoIdentityProvider.Types.CompromisedCredentialsEventActionType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The compromised credentials actions type
--
-- /See:/ 'mkCompromisedCredentialsActionsType' smart constructor.
newtype CompromisedCredentialsActionsType = CompromisedCredentialsActionsType'
  { -- | The event action.
    eventAction :: Types.CompromisedCredentialsEventActionType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CompromisedCredentialsActionsType' value with any optional fields omitted.
mkCompromisedCredentialsActionsType ::
  -- | 'eventAction'
  Types.CompromisedCredentialsEventActionType ->
  CompromisedCredentialsActionsType
mkCompromisedCredentialsActionsType eventAction =
  CompromisedCredentialsActionsType' {eventAction}

-- | The event action.
--
-- /Note:/ Consider using 'eventAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccatEventAction :: Lens.Lens' CompromisedCredentialsActionsType Types.CompromisedCredentialsEventActionType
ccatEventAction = Lens.field @"eventAction"
{-# DEPRECATED ccatEventAction "Use generic-lens or generic-optics with 'eventAction' instead." #-}

instance Core.FromJSON CompromisedCredentialsActionsType where
  toJSON CompromisedCredentialsActionsType {..} =
    Core.object
      (Core.catMaybes [Core.Just ("EventAction" Core..= eventAction)])

instance Core.FromJSON CompromisedCredentialsActionsType where
  parseJSON =
    Core.withObject "CompromisedCredentialsActionsType" Core.$
      \x ->
        CompromisedCredentialsActionsType'
          Core.<$> (x Core..: "EventAction")

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

import Network.AWS.CognitoIdentityProvider.Types.CompromisedCredentialsEventActionType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The compromised credentials actions type
--
-- /See:/ 'mkCompromisedCredentialsActionsType' smart constructor.
newtype CompromisedCredentialsActionsType = CompromisedCredentialsActionsType'
  { eventAction ::
      CompromisedCredentialsEventActionType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CompromisedCredentialsActionsType' with the minimum fields required to make a request.
--
-- * 'eventAction' - The event action.
mkCompromisedCredentialsActionsType ::
  -- | 'eventAction'
  CompromisedCredentialsEventActionType ->
  CompromisedCredentialsActionsType
mkCompromisedCredentialsActionsType pEventAction_ =
  CompromisedCredentialsActionsType' {eventAction = pEventAction_}

-- | The event action.
--
-- /Note:/ Consider using 'eventAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccatEventAction :: Lens.Lens' CompromisedCredentialsActionsType CompromisedCredentialsEventActionType
ccatEventAction = Lens.lens (eventAction :: CompromisedCredentialsActionsType -> CompromisedCredentialsEventActionType) (\s a -> s {eventAction = a} :: CompromisedCredentialsActionsType)
{-# DEPRECATED ccatEventAction "Use generic-lens or generic-optics with 'eventAction' instead." #-}

instance Lude.FromJSON CompromisedCredentialsActionsType where
  parseJSON =
    Lude.withObject
      "CompromisedCredentialsActionsType"
      ( \x ->
          CompromisedCredentialsActionsType'
            Lude.<$> (x Lude..: "EventAction")
      )

instance Lude.ToJSON CompromisedCredentialsActionsType where
  toJSON CompromisedCredentialsActionsType' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("EventAction" Lude..= eventAction)])

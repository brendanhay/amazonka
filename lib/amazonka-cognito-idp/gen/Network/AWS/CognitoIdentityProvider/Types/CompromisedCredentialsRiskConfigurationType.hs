{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.CompromisedCredentialsRiskConfigurationType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.CompromisedCredentialsRiskConfigurationType
  ( CompromisedCredentialsRiskConfigurationType (..),

    -- * Smart constructor
    mkCompromisedCredentialsRiskConfigurationType,

    -- * Lenses
    ccrctEventFilter,
    ccrctActions,
  )
where

import Network.AWS.CognitoIdentityProvider.Types.CompromisedCredentialsActionsType
import Network.AWS.CognitoIdentityProvider.Types.EventFilterType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The compromised credentials risk configuration type.
--
-- /See:/ 'mkCompromisedCredentialsRiskConfigurationType' smart constructor.
data CompromisedCredentialsRiskConfigurationType = CompromisedCredentialsRiskConfigurationType'
  { eventFilter ::
      Lude.Maybe
        [EventFilterType],
    actions ::
      CompromisedCredentialsActionsType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CompromisedCredentialsRiskConfigurationType' with the minimum fields required to make a request.
--
-- * 'actions' - The compromised credentials risk configuration actions.
-- * 'eventFilter' - Perform the action for these events. The default is to perform all events if no event filter is specified.
mkCompromisedCredentialsRiskConfigurationType ::
  -- | 'actions'
  CompromisedCredentialsActionsType ->
  CompromisedCredentialsRiskConfigurationType
mkCompromisedCredentialsRiskConfigurationType pActions_ =
  CompromisedCredentialsRiskConfigurationType'
    { eventFilter =
        Lude.Nothing,
      actions = pActions_
    }

-- | Perform the action for these events. The default is to perform all events if no event filter is specified.
--
-- /Note:/ Consider using 'eventFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrctEventFilter :: Lens.Lens' CompromisedCredentialsRiskConfigurationType (Lude.Maybe [EventFilterType])
ccrctEventFilter = Lens.lens (eventFilter :: CompromisedCredentialsRiskConfigurationType -> Lude.Maybe [EventFilterType]) (\s a -> s {eventFilter = a} :: CompromisedCredentialsRiskConfigurationType)
{-# DEPRECATED ccrctEventFilter "Use generic-lens or generic-optics with 'eventFilter' instead." #-}

-- | The compromised credentials risk configuration actions.
--
-- /Note:/ Consider using 'actions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrctActions :: Lens.Lens' CompromisedCredentialsRiskConfigurationType CompromisedCredentialsActionsType
ccrctActions = Lens.lens (actions :: CompromisedCredentialsRiskConfigurationType -> CompromisedCredentialsActionsType) (\s a -> s {actions = a} :: CompromisedCredentialsRiskConfigurationType)
{-# DEPRECATED ccrctActions "Use generic-lens or generic-optics with 'actions' instead." #-}

instance Lude.FromJSON CompromisedCredentialsRiskConfigurationType where
  parseJSON =
    Lude.withObject
      "CompromisedCredentialsRiskConfigurationType"
      ( \x ->
          CompromisedCredentialsRiskConfigurationType'
            Lude.<$> (x Lude..:? "EventFilter" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..: "Actions")
      )

instance Lude.ToJSON CompromisedCredentialsRiskConfigurationType where
  toJSON CompromisedCredentialsRiskConfigurationType' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("EventFilter" Lude..=) Lude.<$> eventFilter,
            Lude.Just ("Actions" Lude..= actions)
          ]
      )

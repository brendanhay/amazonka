{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.AccountTakeoverRiskConfigurationType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.AccountTakeoverRiskConfigurationType
  ( AccountTakeoverRiskConfigurationType (..),

    -- * Smart constructor
    mkAccountTakeoverRiskConfigurationType,

    -- * Lenses
    atrctNotifyConfiguration,
    atrctActions,
  )
where

import Network.AWS.CognitoIdentityProvider.Types.AccountTakeoverActionsType
import Network.AWS.CognitoIdentityProvider.Types.NotifyConfigurationType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Configuration for mitigation actions and notification for different levels of risk detected for a potential account takeover.
--
-- /See:/ 'mkAccountTakeoverRiskConfigurationType' smart constructor.
data AccountTakeoverRiskConfigurationType = AccountTakeoverRiskConfigurationType'
  { notifyConfiguration ::
      Lude.Maybe
        NotifyConfigurationType,
    actions ::
      AccountTakeoverActionsType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AccountTakeoverRiskConfigurationType' with the minimum fields required to make a request.
--
-- * 'actions' - Account takeover risk configuration actions
-- * 'notifyConfiguration' - The notify configuration used to construct email notifications.
mkAccountTakeoverRiskConfigurationType ::
  -- | 'actions'
  AccountTakeoverActionsType ->
  AccountTakeoverRiskConfigurationType
mkAccountTakeoverRiskConfigurationType pActions_ =
  AccountTakeoverRiskConfigurationType'
    { notifyConfiguration =
        Lude.Nothing,
      actions = pActions_
    }

-- | The notify configuration used to construct email notifications.
--
-- /Note:/ Consider using 'notifyConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atrctNotifyConfiguration :: Lens.Lens' AccountTakeoverRiskConfigurationType (Lude.Maybe NotifyConfigurationType)
atrctNotifyConfiguration = Lens.lens (notifyConfiguration :: AccountTakeoverRiskConfigurationType -> Lude.Maybe NotifyConfigurationType) (\s a -> s {notifyConfiguration = a} :: AccountTakeoverRiskConfigurationType)
{-# DEPRECATED atrctNotifyConfiguration "Use generic-lens or generic-optics with 'notifyConfiguration' instead." #-}

-- | Account takeover risk configuration actions
--
-- /Note:/ Consider using 'actions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atrctActions :: Lens.Lens' AccountTakeoverRiskConfigurationType AccountTakeoverActionsType
atrctActions = Lens.lens (actions :: AccountTakeoverRiskConfigurationType -> AccountTakeoverActionsType) (\s a -> s {actions = a} :: AccountTakeoverRiskConfigurationType)
{-# DEPRECATED atrctActions "Use generic-lens or generic-optics with 'actions' instead." #-}

instance Lude.FromJSON AccountTakeoverRiskConfigurationType where
  parseJSON =
    Lude.withObject
      "AccountTakeoverRiskConfigurationType"
      ( \x ->
          AccountTakeoverRiskConfigurationType'
            Lude.<$> (x Lude..:? "NotifyConfiguration") Lude.<*> (x Lude..: "Actions")
      )

instance Lude.ToJSON AccountTakeoverRiskConfigurationType where
  toJSON AccountTakeoverRiskConfigurationType' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NotifyConfiguration" Lude..=) Lude.<$> notifyConfiguration,
            Lude.Just ("Actions" Lude..= actions)
          ]
      )

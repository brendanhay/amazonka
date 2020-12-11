-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.AccountTakeoverActionsType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.AccountTakeoverActionsType
  ( AccountTakeoverActionsType (..),

    -- * Smart constructor
    mkAccountTakeoverActionsType,

    -- * Lenses
    atatLowAction,
    atatHighAction,
    atatMediumAction,
  )
where

import Network.AWS.CognitoIdentityProvider.Types.AccountTakeoverActionType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Account takeover actions type.
--
-- /See:/ 'mkAccountTakeoverActionsType' smart constructor.
data AccountTakeoverActionsType = AccountTakeoverActionsType'
  { lowAction ::
      Lude.Maybe AccountTakeoverActionType,
    highAction ::
      Lude.Maybe AccountTakeoverActionType,
    mediumAction ::
      Lude.Maybe AccountTakeoverActionType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AccountTakeoverActionsType' with the minimum fields required to make a request.
--
-- * 'highAction' - Action to take for a high risk.
-- * 'lowAction' - Action to take for a low risk.
-- * 'mediumAction' - Action to take for a medium risk.
mkAccountTakeoverActionsType ::
  AccountTakeoverActionsType
mkAccountTakeoverActionsType =
  AccountTakeoverActionsType'
    { lowAction = Lude.Nothing,
      highAction = Lude.Nothing,
      mediumAction = Lude.Nothing
    }

-- | Action to take for a low risk.
--
-- /Note:/ Consider using 'lowAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atatLowAction :: Lens.Lens' AccountTakeoverActionsType (Lude.Maybe AccountTakeoverActionType)
atatLowAction = Lens.lens (lowAction :: AccountTakeoverActionsType -> Lude.Maybe AccountTakeoverActionType) (\s a -> s {lowAction = a} :: AccountTakeoverActionsType)
{-# DEPRECATED atatLowAction "Use generic-lens or generic-optics with 'lowAction' instead." #-}

-- | Action to take for a high risk.
--
-- /Note:/ Consider using 'highAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atatHighAction :: Lens.Lens' AccountTakeoverActionsType (Lude.Maybe AccountTakeoverActionType)
atatHighAction = Lens.lens (highAction :: AccountTakeoverActionsType -> Lude.Maybe AccountTakeoverActionType) (\s a -> s {highAction = a} :: AccountTakeoverActionsType)
{-# DEPRECATED atatHighAction "Use generic-lens or generic-optics with 'highAction' instead." #-}

-- | Action to take for a medium risk.
--
-- /Note:/ Consider using 'mediumAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atatMediumAction :: Lens.Lens' AccountTakeoverActionsType (Lude.Maybe AccountTakeoverActionType)
atatMediumAction = Lens.lens (mediumAction :: AccountTakeoverActionsType -> Lude.Maybe AccountTakeoverActionType) (\s a -> s {mediumAction = a} :: AccountTakeoverActionsType)
{-# DEPRECATED atatMediumAction "Use generic-lens or generic-optics with 'mediumAction' instead." #-}

instance Lude.FromJSON AccountTakeoverActionsType where
  parseJSON =
    Lude.withObject
      "AccountTakeoverActionsType"
      ( \x ->
          AccountTakeoverActionsType'
            Lude.<$> (x Lude..:? "LowAction")
            Lude.<*> (x Lude..:? "HighAction")
            Lude.<*> (x Lude..:? "MediumAction")
      )

instance Lude.ToJSON AccountTakeoverActionsType where
  toJSON AccountTakeoverActionsType' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("LowAction" Lude..=) Lude.<$> lowAction,
            ("HighAction" Lude..=) Lude.<$> highAction,
            ("MediumAction" Lude..=) Lude.<$> mediumAction
          ]
      )

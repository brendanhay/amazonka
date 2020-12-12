{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.Types.ActionThreshold
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Budgets.Types.ActionThreshold
  ( ActionThreshold (..),

    -- * Smart constructor
    mkActionThreshold,

    -- * Lenses
    atActionThresholdValue,
    atActionThresholdType,
  )
where

import Network.AWS.Budgets.Types.ThresholdType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The trigger threshold of the action.
--
-- /See:/ 'mkActionThreshold' smart constructor.
data ActionThreshold = ActionThreshold'
  { actionThresholdValue ::
      Lude.Double,
    actionThresholdType :: ThresholdType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ActionThreshold' with the minimum fields required to make a request.
--
-- * 'actionThresholdType' - Undocumented field.
-- * 'actionThresholdValue' - Undocumented field.
mkActionThreshold ::
  -- | 'actionThresholdValue'
  Lude.Double ->
  -- | 'actionThresholdType'
  ThresholdType ->
  ActionThreshold
mkActionThreshold pActionThresholdValue_ pActionThresholdType_ =
  ActionThreshold'
    { actionThresholdValue = pActionThresholdValue_,
      actionThresholdType = pActionThresholdType_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'actionThresholdValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atActionThresholdValue :: Lens.Lens' ActionThreshold Lude.Double
atActionThresholdValue = Lens.lens (actionThresholdValue :: ActionThreshold -> Lude.Double) (\s a -> s {actionThresholdValue = a} :: ActionThreshold)
{-# DEPRECATED atActionThresholdValue "Use generic-lens or generic-optics with 'actionThresholdValue' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'actionThresholdType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atActionThresholdType :: Lens.Lens' ActionThreshold ThresholdType
atActionThresholdType = Lens.lens (actionThresholdType :: ActionThreshold -> ThresholdType) (\s a -> s {actionThresholdType = a} :: ActionThreshold)
{-# DEPRECATED atActionThresholdType "Use generic-lens or generic-optics with 'actionThresholdType' instead." #-}

instance Lude.FromJSON ActionThreshold where
  parseJSON =
    Lude.withObject
      "ActionThreshold"
      ( \x ->
          ActionThreshold'
            Lude.<$> (x Lude..: "ActionThresholdValue")
            Lude.<*> (x Lude..: "ActionThresholdType")
      )

instance Lude.ToJSON ActionThreshold where
  toJSON ActionThreshold' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ActionThresholdValue" Lude..= actionThresholdValue),
            Lude.Just ("ActionThresholdType" Lude..= actionThresholdType)
          ]
      )

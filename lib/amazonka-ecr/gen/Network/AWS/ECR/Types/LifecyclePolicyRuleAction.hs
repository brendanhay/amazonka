{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.Types.LifecyclePolicyRuleAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECR.Types.LifecyclePolicyRuleAction
  ( LifecyclePolicyRuleAction (..),

    -- * Smart constructor
    mkLifecyclePolicyRuleAction,

    -- * Lenses
    lpraType,
  )
where

import Network.AWS.ECR.Types.ImageActionType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The type of action to be taken.
--
-- /See:/ 'mkLifecyclePolicyRuleAction' smart constructor.
newtype LifecyclePolicyRuleAction = LifecyclePolicyRuleAction'
  { -- | The type of action to be taken.
    type' :: Lude.Maybe ImageActionType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LifecyclePolicyRuleAction' with the minimum fields required to make a request.
--
-- * 'type'' - The type of action to be taken.
mkLifecyclePolicyRuleAction ::
  LifecyclePolicyRuleAction
mkLifecyclePolicyRuleAction =
  LifecyclePolicyRuleAction' {type' = Lude.Nothing}

-- | The type of action to be taken.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpraType :: Lens.Lens' LifecyclePolicyRuleAction (Lude.Maybe ImageActionType)
lpraType = Lens.lens (type' :: LifecyclePolicyRuleAction -> Lude.Maybe ImageActionType) (\s a -> s {type' = a} :: LifecyclePolicyRuleAction)
{-# DEPRECATED lpraType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromJSON LifecyclePolicyRuleAction where
  parseJSON =
    Lude.withObject
      "LifecyclePolicyRuleAction"
      (\x -> LifecyclePolicyRuleAction' Lude.<$> (x Lude..:? "type"))

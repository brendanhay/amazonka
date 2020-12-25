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

import qualified Network.AWS.ECR.Types.ImageActionType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The type of action to be taken.
--
-- /See:/ 'mkLifecyclePolicyRuleAction' smart constructor.
newtype LifecyclePolicyRuleAction = LifecyclePolicyRuleAction'
  { -- | The type of action to be taken.
    type' :: Core.Maybe Types.ImageActionType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'LifecyclePolicyRuleAction' value with any optional fields omitted.
mkLifecyclePolicyRuleAction ::
  LifecyclePolicyRuleAction
mkLifecyclePolicyRuleAction =
  LifecyclePolicyRuleAction' {type' = Core.Nothing}

-- | The type of action to be taken.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpraType :: Lens.Lens' LifecyclePolicyRuleAction (Core.Maybe Types.ImageActionType)
lpraType = Lens.field @"type'"
{-# DEPRECATED lpraType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Core.FromJSON LifecyclePolicyRuleAction where
  parseJSON =
    Core.withObject "LifecyclePolicyRuleAction" Core.$
      \x -> LifecyclePolicyRuleAction' Core.<$> (x Core..:? "type")

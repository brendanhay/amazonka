{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.ScalingRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.ScalingRule
  ( ScalingRule (..),

    -- * Smart constructor
    mkScalingRule,

    -- * Lenses
    srDescription,
    srName,
    srAction,
    srTrigger,
  )
where

import Network.AWS.EMR.Types.ScalingAction
import Network.AWS.EMR.Types.ScalingTrigger
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A scale-in or scale-out rule that defines scaling activity, including the CloudWatch metric alarm that triggers activity, how EC2 instances are added or removed, and the periodicity of adjustments. The automatic scaling policy for an instance group can comprise one or more automatic scaling rules.
--
-- /See:/ 'mkScalingRule' smart constructor.
data ScalingRule = ScalingRule'
  { description ::
      Lude.Maybe Lude.Text,
    name :: Lude.Text,
    action :: ScalingAction,
    trigger :: ScalingTrigger
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ScalingRule' with the minimum fields required to make a request.
--
-- * 'action' - The conditions that trigger an automatic scaling activity.
-- * 'description' - A friendly, more verbose description of the automatic scaling rule.
-- * 'name' - The name used to identify an automatic scaling rule. Rule names must be unique within a scaling policy.
-- * 'trigger' - The CloudWatch alarm definition that determines when automatic scaling activity is triggered.
mkScalingRule ::
  -- | 'name'
  Lude.Text ->
  -- | 'action'
  ScalingAction ->
  -- | 'trigger'
  ScalingTrigger ->
  ScalingRule
mkScalingRule pName_ pAction_ pTrigger_ =
  ScalingRule'
    { description = Lude.Nothing,
      name = pName_,
      action = pAction_,
      trigger = pTrigger_
    }

-- | A friendly, more verbose description of the automatic scaling rule.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srDescription :: Lens.Lens' ScalingRule (Lude.Maybe Lude.Text)
srDescription = Lens.lens (description :: ScalingRule -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: ScalingRule)
{-# DEPRECATED srDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The name used to identify an automatic scaling rule. Rule names must be unique within a scaling policy.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srName :: Lens.Lens' ScalingRule Lude.Text
srName = Lens.lens (name :: ScalingRule -> Lude.Text) (\s a -> s {name = a} :: ScalingRule)
{-# DEPRECATED srName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The conditions that trigger an automatic scaling activity.
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srAction :: Lens.Lens' ScalingRule ScalingAction
srAction = Lens.lens (action :: ScalingRule -> ScalingAction) (\s a -> s {action = a} :: ScalingRule)
{-# DEPRECATED srAction "Use generic-lens or generic-optics with 'action' instead." #-}

-- | The CloudWatch alarm definition that determines when automatic scaling activity is triggered.
--
-- /Note:/ Consider using 'trigger' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srTrigger :: Lens.Lens' ScalingRule ScalingTrigger
srTrigger = Lens.lens (trigger :: ScalingRule -> ScalingTrigger) (\s a -> s {trigger = a} :: ScalingRule)
{-# DEPRECATED srTrigger "Use generic-lens or generic-optics with 'trigger' instead." #-}

instance Lude.FromJSON ScalingRule where
  parseJSON =
    Lude.withObject
      "ScalingRule"
      ( \x ->
          ScalingRule'
            Lude.<$> (x Lude..:? "Description")
            Lude.<*> (x Lude..: "Name")
            Lude.<*> (x Lude..: "Action")
            Lude.<*> (x Lude..: "Trigger")
      )

instance Lude.ToJSON ScalingRule where
  toJSON ScalingRule' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Description" Lude..=) Lude.<$> description,
            Lude.Just ("Name" Lude..= name),
            Lude.Just ("Action" Lude..= action),
            Lude.Just ("Trigger" Lude..= trigger)
          ]
      )

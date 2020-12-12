{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.LateDataRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.LateDataRule
  ( LateDataRule (..),

    -- * Smart constructor
    mkLateDataRule,

    -- * Lenses
    ldrRuleName,
    ldrRuleConfiguration,
  )
where

import Network.AWS.IoTAnalytics.Types.LateDataRuleConfiguration
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A structure that contains the name and configuration information of a late data rule.
--
-- /See:/ 'mkLateDataRule' smart constructor.
data LateDataRule = LateDataRule'
  { ruleName :: Lude.Maybe Lude.Text,
    ruleConfiguration :: LateDataRuleConfiguration
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LateDataRule' with the minimum fields required to make a request.
--
-- * 'ruleConfiguration' - The information needed to configure the late data rule.
-- * 'ruleName' - The name of the late data rule.
mkLateDataRule ::
  -- | 'ruleConfiguration'
  LateDataRuleConfiguration ->
  LateDataRule
mkLateDataRule pRuleConfiguration_ =
  LateDataRule'
    { ruleName = Lude.Nothing,
      ruleConfiguration = pRuleConfiguration_
    }

-- | The name of the late data rule.
--
-- /Note:/ Consider using 'ruleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrRuleName :: Lens.Lens' LateDataRule (Lude.Maybe Lude.Text)
ldrRuleName = Lens.lens (ruleName :: LateDataRule -> Lude.Maybe Lude.Text) (\s a -> s {ruleName = a} :: LateDataRule)
{-# DEPRECATED ldrRuleName "Use generic-lens or generic-optics with 'ruleName' instead." #-}

-- | The information needed to configure the late data rule.
--
-- /Note:/ Consider using 'ruleConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrRuleConfiguration :: Lens.Lens' LateDataRule LateDataRuleConfiguration
ldrRuleConfiguration = Lens.lens (ruleConfiguration :: LateDataRule -> LateDataRuleConfiguration) (\s a -> s {ruleConfiguration = a} :: LateDataRule)
{-# DEPRECATED ldrRuleConfiguration "Use generic-lens or generic-optics with 'ruleConfiguration' instead." #-}

instance Lude.FromJSON LateDataRule where
  parseJSON =
    Lude.withObject
      "LateDataRule"
      ( \x ->
          LateDataRule'
            Lude.<$> (x Lude..:? "ruleName") Lude.<*> (x Lude..: "ruleConfiguration")
      )

instance Lude.ToJSON LateDataRule where
  toJSON LateDataRule' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ruleName" Lude..=) Lude.<$> ruleName,
            Lude.Just ("ruleConfiguration" Lude..= ruleConfiguration)
          ]
      )

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ComplianceByConfigRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ComplianceByConfigRule
  ( ComplianceByConfigRule (..),

    -- * Smart constructor
    mkComplianceByConfigRule,

    -- * Lenses
    cbcrCompliance,
    cbcrConfigRuleName,
  )
where

import Network.AWS.Config.Types.Compliance
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Indicates whether an AWS Config rule is compliant. A rule is compliant if all of the resources that the rule evaluated comply with it. A rule is noncompliant if any of these resources do not comply.
--
-- /See:/ 'mkComplianceByConfigRule' smart constructor.
data ComplianceByConfigRule = ComplianceByConfigRule'
  { -- | Indicates whether the AWS Config rule is compliant.
    compliance :: Lude.Maybe Compliance,
    -- | The name of the AWS Config rule.
    configRuleName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ComplianceByConfigRule' with the minimum fields required to make a request.
--
-- * 'compliance' - Indicates whether the AWS Config rule is compliant.
-- * 'configRuleName' - The name of the AWS Config rule.
mkComplianceByConfigRule ::
  ComplianceByConfigRule
mkComplianceByConfigRule =
  ComplianceByConfigRule'
    { compliance = Lude.Nothing,
      configRuleName = Lude.Nothing
    }

-- | Indicates whether the AWS Config rule is compliant.
--
-- /Note:/ Consider using 'compliance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbcrCompliance :: Lens.Lens' ComplianceByConfigRule (Lude.Maybe Compliance)
cbcrCompliance = Lens.lens (compliance :: ComplianceByConfigRule -> Lude.Maybe Compliance) (\s a -> s {compliance = a} :: ComplianceByConfigRule)
{-# DEPRECATED cbcrCompliance "Use generic-lens or generic-optics with 'compliance' instead." #-}

-- | The name of the AWS Config rule.
--
-- /Note:/ Consider using 'configRuleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbcrConfigRuleName :: Lens.Lens' ComplianceByConfigRule (Lude.Maybe Lude.Text)
cbcrConfigRuleName = Lens.lens (configRuleName :: ComplianceByConfigRule -> Lude.Maybe Lude.Text) (\s a -> s {configRuleName = a} :: ComplianceByConfigRule)
{-# DEPRECATED cbcrConfigRuleName "Use generic-lens or generic-optics with 'configRuleName' instead." #-}

instance Lude.FromJSON ComplianceByConfigRule where
  parseJSON =
    Lude.withObject
      "ComplianceByConfigRule"
      ( \x ->
          ComplianceByConfigRule'
            Lude.<$> (x Lude..:? "Compliance") Lude.<*> (x Lude..:? "ConfigRuleName")
      )

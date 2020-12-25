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

import qualified Network.AWS.Config.Types.Compliance as Types
import qualified Network.AWS.Config.Types.StringWithCharLimit64 as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Indicates whether an AWS Config rule is compliant. A rule is compliant if all of the resources that the rule evaluated comply with it. A rule is noncompliant if any of these resources do not comply.
--
-- /See:/ 'mkComplianceByConfigRule' smart constructor.
data ComplianceByConfigRule = ComplianceByConfigRule'
  { -- | Indicates whether the AWS Config rule is compliant.
    compliance :: Core.Maybe Types.Compliance,
    -- | The name of the AWS Config rule.
    configRuleName :: Core.Maybe Types.StringWithCharLimit64
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ComplianceByConfigRule' value with any optional fields omitted.
mkComplianceByConfigRule ::
  ComplianceByConfigRule
mkComplianceByConfigRule =
  ComplianceByConfigRule'
    { compliance = Core.Nothing,
      configRuleName = Core.Nothing
    }

-- | Indicates whether the AWS Config rule is compliant.
--
-- /Note:/ Consider using 'compliance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbcrCompliance :: Lens.Lens' ComplianceByConfigRule (Core.Maybe Types.Compliance)
cbcrCompliance = Lens.field @"compliance"
{-# DEPRECATED cbcrCompliance "Use generic-lens or generic-optics with 'compliance' instead." #-}

-- | The name of the AWS Config rule.
--
-- /Note:/ Consider using 'configRuleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbcrConfigRuleName :: Lens.Lens' ComplianceByConfigRule (Core.Maybe Types.StringWithCharLimit64)
cbcrConfigRuleName = Lens.field @"configRuleName"
{-# DEPRECATED cbcrConfigRuleName "Use generic-lens or generic-optics with 'configRuleName' instead." #-}

instance Core.FromJSON ComplianceByConfigRule where
  parseJSON =
    Core.withObject "ComplianceByConfigRule" Core.$
      \x ->
        ComplianceByConfigRule'
          Core.<$> (x Core..:? "Compliance") Core.<*> (x Core..:? "ConfigRuleName")

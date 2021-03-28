{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ConformancePackRuleCompliance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Config.Types.ConformancePackRuleCompliance
  ( ConformancePackRuleCompliance (..)
  -- * Smart constructor
  , mkConformancePackRuleCompliance
  -- * Lenses
  , cprcComplianceType
  , cprcConfigRuleName
  ) where

import qualified Network.AWS.Config.Types.ConfigRuleName as Types
import qualified Network.AWS.Config.Types.ConformancePackComplianceType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Compliance information of one or more AWS Config rules within a conformance pack. You can filter using AWS Config rule names and compliance types.
--
-- /See:/ 'mkConformancePackRuleCompliance' smart constructor.
data ConformancePackRuleCompliance = ConformancePackRuleCompliance'
  { complianceType :: Core.Maybe Types.ConformancePackComplianceType
    -- ^ Compliance of the AWS Config rule
--
-- The allowed values are @COMPLIANT@ and @NON_COMPLIANT@ .
  , configRuleName :: Core.Maybe Types.ConfigRuleName
    -- ^ Name of the config rule.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ConformancePackRuleCompliance' value with any optional fields omitted.
mkConformancePackRuleCompliance
    :: ConformancePackRuleCompliance
mkConformancePackRuleCompliance
  = ConformancePackRuleCompliance'{complianceType = Core.Nothing,
                                   configRuleName = Core.Nothing}

-- | Compliance of the AWS Config rule
--
-- The allowed values are @COMPLIANT@ and @NON_COMPLIANT@ .
--
-- /Note:/ Consider using 'complianceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprcComplianceType :: Lens.Lens' ConformancePackRuleCompliance (Core.Maybe Types.ConformancePackComplianceType)
cprcComplianceType = Lens.field @"complianceType"
{-# INLINEABLE cprcComplianceType #-}
{-# DEPRECATED complianceType "Use generic-lens or generic-optics with 'complianceType' instead"  #-}

-- | Name of the config rule.
--
-- /Note:/ Consider using 'configRuleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprcConfigRuleName :: Lens.Lens' ConformancePackRuleCompliance (Core.Maybe Types.ConfigRuleName)
cprcConfigRuleName = Lens.field @"configRuleName"
{-# INLINEABLE cprcConfigRuleName #-}
{-# DEPRECATED configRuleName "Use generic-lens or generic-optics with 'configRuleName' instead"  #-}

instance Core.FromJSON ConformancePackRuleCompliance where
        parseJSON
          = Core.withObject "ConformancePackRuleCompliance" Core.$
              \ x ->
                ConformancePackRuleCompliance' Core.<$>
                  (x Core..:? "ComplianceType") Core.<*> x Core..:? "ConfigRuleName"

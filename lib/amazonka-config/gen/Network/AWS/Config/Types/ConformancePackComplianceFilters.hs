{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ConformancePackComplianceFilters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ConformancePackComplianceFilters
  ( ConformancePackComplianceFilters (..),

    -- * Smart constructor
    mkConformancePackComplianceFilters,

    -- * Lenses
    cpcfComplianceType,
    cpcfConfigRuleNames,
  )
where

import qualified Network.AWS.Config.Types.ConformancePackComplianceType as Types
import qualified Network.AWS.Config.Types.StringWithCharLimit64 as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Filters the conformance pack by compliance types and AWS Config rule names.
--
-- /See:/ 'mkConformancePackComplianceFilters' smart constructor.
data ConformancePackComplianceFilters = ConformancePackComplianceFilters'
  { -- | Filters the results by compliance.
    --
    -- The allowed values are @COMPLIANT@ and @NON_COMPLIANT@ .
    complianceType :: Core.Maybe Types.ConformancePackComplianceType,
    -- | Filters the results by AWS Config rule names.
    configRuleNames :: Core.Maybe [Types.StringWithCharLimit64]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ConformancePackComplianceFilters' value with any optional fields omitted.
mkConformancePackComplianceFilters ::
  ConformancePackComplianceFilters
mkConformancePackComplianceFilters =
  ConformancePackComplianceFilters'
    { complianceType = Core.Nothing,
      configRuleNames = Core.Nothing
    }

-- | Filters the results by compliance.
--
-- The allowed values are @COMPLIANT@ and @NON_COMPLIANT@ .
--
-- /Note:/ Consider using 'complianceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpcfComplianceType :: Lens.Lens' ConformancePackComplianceFilters (Core.Maybe Types.ConformancePackComplianceType)
cpcfComplianceType = Lens.field @"complianceType"
{-# DEPRECATED cpcfComplianceType "Use generic-lens or generic-optics with 'complianceType' instead." #-}

-- | Filters the results by AWS Config rule names.
--
-- /Note:/ Consider using 'configRuleNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpcfConfigRuleNames :: Lens.Lens' ConformancePackComplianceFilters (Core.Maybe [Types.StringWithCharLimit64])
cpcfConfigRuleNames = Lens.field @"configRuleNames"
{-# DEPRECATED cpcfConfigRuleNames "Use generic-lens or generic-optics with 'configRuleNames' instead." #-}

instance Core.FromJSON ConformancePackComplianceFilters where
  toJSON ConformancePackComplianceFilters {..} =
    Core.object
      ( Core.catMaybes
          [ ("ComplianceType" Core..=) Core.<$> complianceType,
            ("ConfigRuleNames" Core..=) Core.<$> configRuleNames
          ]
      )

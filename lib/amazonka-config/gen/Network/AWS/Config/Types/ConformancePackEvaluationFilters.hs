{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ConformancePackEvaluationFilters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Config.Types.ConformancePackEvaluationFilters
  ( ConformancePackEvaluationFilters (..)
  -- * Smart constructor
  , mkConformancePackEvaluationFilters
  -- * Lenses
  , cpefComplianceType
  , cpefConfigRuleNames
  , cpefResourceIds
  , cpefResourceType
  ) where

import qualified Network.AWS.Config.Types.ConformancePackComplianceType as Types
import qualified Network.AWS.Config.Types.StringWithCharLimit256 as Types
import qualified Network.AWS.Config.Types.StringWithCharLimit64 as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Filters a conformance pack by AWS Config rule names, compliance types, AWS resource types, and resource IDs.
--
-- /See:/ 'mkConformancePackEvaluationFilters' smart constructor.
data ConformancePackEvaluationFilters = ConformancePackEvaluationFilters'
  { complianceType :: Core.Maybe Types.ConformancePackComplianceType
    -- ^ Filters the results by compliance.
--
-- The allowed values are @COMPLIANT@ and @NON_COMPLIANT@ .
  , configRuleNames :: Core.Maybe [Types.StringWithCharLimit64]
    -- ^ Filters the results by AWS Config rule names.
  , resourceIds :: Core.Maybe [Types.StringWithCharLimit256]
    -- ^ Filters the results by resource IDs.
  , resourceType :: Core.Maybe Types.StringWithCharLimit256
    -- ^ Filters the results by the resource type (for example, @"AWS::EC2::Instance"@ ). 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ConformancePackEvaluationFilters' value with any optional fields omitted.
mkConformancePackEvaluationFilters
    :: ConformancePackEvaluationFilters
mkConformancePackEvaluationFilters
  = ConformancePackEvaluationFilters'{complianceType = Core.Nothing,
                                      configRuleNames = Core.Nothing, resourceIds = Core.Nothing,
                                      resourceType = Core.Nothing}

-- | Filters the results by compliance.
--
-- The allowed values are @COMPLIANT@ and @NON_COMPLIANT@ .
--
-- /Note:/ Consider using 'complianceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpefComplianceType :: Lens.Lens' ConformancePackEvaluationFilters (Core.Maybe Types.ConformancePackComplianceType)
cpefComplianceType = Lens.field @"complianceType"
{-# INLINEABLE cpefComplianceType #-}
{-# DEPRECATED complianceType "Use generic-lens or generic-optics with 'complianceType' instead"  #-}

-- | Filters the results by AWS Config rule names.
--
-- /Note:/ Consider using 'configRuleNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpefConfigRuleNames :: Lens.Lens' ConformancePackEvaluationFilters (Core.Maybe [Types.StringWithCharLimit64])
cpefConfigRuleNames = Lens.field @"configRuleNames"
{-# INLINEABLE cpefConfigRuleNames #-}
{-# DEPRECATED configRuleNames "Use generic-lens or generic-optics with 'configRuleNames' instead"  #-}

-- | Filters the results by resource IDs.
--
-- /Note:/ Consider using 'resourceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpefResourceIds :: Lens.Lens' ConformancePackEvaluationFilters (Core.Maybe [Types.StringWithCharLimit256])
cpefResourceIds = Lens.field @"resourceIds"
{-# INLINEABLE cpefResourceIds #-}
{-# DEPRECATED resourceIds "Use generic-lens or generic-optics with 'resourceIds' instead"  #-}

-- | Filters the results by the resource type (for example, @"AWS::EC2::Instance"@ ). 
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpefResourceType :: Lens.Lens' ConformancePackEvaluationFilters (Core.Maybe Types.StringWithCharLimit256)
cpefResourceType = Lens.field @"resourceType"
{-# INLINEABLE cpefResourceType #-}
{-# DEPRECATED resourceType "Use generic-lens or generic-optics with 'resourceType' instead"  #-}

instance Core.FromJSON ConformancePackEvaluationFilters where
        toJSON ConformancePackEvaluationFilters{..}
          = Core.object
              (Core.catMaybes
                 [("ComplianceType" Core..=) Core.<$> complianceType,
                  ("ConfigRuleNames" Core..=) Core.<$> configRuleNames,
                  ("ResourceIds" Core..=) Core.<$> resourceIds,
                  ("ResourceType" Core..=) Core.<$> resourceType])

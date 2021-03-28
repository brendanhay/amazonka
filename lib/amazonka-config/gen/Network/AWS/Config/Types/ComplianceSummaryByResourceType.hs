{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ComplianceSummaryByResourceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Config.Types.ComplianceSummaryByResourceType
  ( ComplianceSummaryByResourceType (..)
  -- * Smart constructor
  , mkComplianceSummaryByResourceType
  -- * Lenses
  , csbrtComplianceSummary
  , csbrtResourceType
  ) where

import qualified Network.AWS.Config.Types.ComplianceSummary as Types
import qualified Network.AWS.Config.Types.StringWithCharLimit256 as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The number of AWS resources of a specific type that are compliant or noncompliant, up to a maximum of 100 for each.
--
-- /See:/ 'mkComplianceSummaryByResourceType' smart constructor.
data ComplianceSummaryByResourceType = ComplianceSummaryByResourceType'
  { complianceSummary :: Core.Maybe Types.ComplianceSummary
    -- ^ The number of AWS resources that are compliant or noncompliant, up to a maximum of 100 for each.
  , resourceType :: Core.Maybe Types.StringWithCharLimit256
    -- ^ The type of AWS resource.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ComplianceSummaryByResourceType' value with any optional fields omitted.
mkComplianceSummaryByResourceType
    :: ComplianceSummaryByResourceType
mkComplianceSummaryByResourceType
  = ComplianceSummaryByResourceType'{complianceSummary =
                                       Core.Nothing,
                                     resourceType = Core.Nothing}

-- | The number of AWS resources that are compliant or noncompliant, up to a maximum of 100 for each.
--
-- /Note:/ Consider using 'complianceSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csbrtComplianceSummary :: Lens.Lens' ComplianceSummaryByResourceType (Core.Maybe Types.ComplianceSummary)
csbrtComplianceSummary = Lens.field @"complianceSummary"
{-# INLINEABLE csbrtComplianceSummary #-}
{-# DEPRECATED complianceSummary "Use generic-lens or generic-optics with 'complianceSummary' instead"  #-}

-- | The type of AWS resource.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csbrtResourceType :: Lens.Lens' ComplianceSummaryByResourceType (Core.Maybe Types.StringWithCharLimit256)
csbrtResourceType = Lens.field @"resourceType"
{-# INLINEABLE csbrtResourceType #-}
{-# DEPRECATED resourceType "Use generic-lens or generic-optics with 'resourceType' instead"  #-}

instance Core.FromJSON ComplianceSummaryByResourceType where
        parseJSON
          = Core.withObject "ComplianceSummaryByResourceType" Core.$
              \ x ->
                ComplianceSummaryByResourceType' Core.<$>
                  (x Core..:? "ComplianceSummary") Core.<*> x Core..:? "ResourceType"

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.AggregateComplianceCount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.AggregateComplianceCount
  ( AggregateComplianceCount (..),

    -- * Smart constructor
    mkAggregateComplianceCount,

    -- * Lenses
    accComplianceSummary,
    accGroupName,
  )
where

import qualified Network.AWS.Config.Types.ComplianceSummary as Types
import qualified Network.AWS.Config.Types.GroupName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Returns the number of compliant and noncompliant rules for one or more accounts and regions in an aggregator.
--
-- /See:/ 'mkAggregateComplianceCount' smart constructor.
data AggregateComplianceCount = AggregateComplianceCount'
  { -- | The number of compliant and noncompliant AWS Config rules.
    complianceSummary :: Core.Maybe Types.ComplianceSummary,
    -- | The 12-digit account ID or region based on the GroupByKey value.
    groupName :: Core.Maybe Types.GroupName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'AggregateComplianceCount' value with any optional fields omitted.
mkAggregateComplianceCount ::
  AggregateComplianceCount
mkAggregateComplianceCount =
  AggregateComplianceCount'
    { complianceSummary = Core.Nothing,
      groupName = Core.Nothing
    }

-- | The number of compliant and noncompliant AWS Config rules.
--
-- /Note:/ Consider using 'complianceSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
accComplianceSummary :: Lens.Lens' AggregateComplianceCount (Core.Maybe Types.ComplianceSummary)
accComplianceSummary = Lens.field @"complianceSummary"
{-# DEPRECATED accComplianceSummary "Use generic-lens or generic-optics with 'complianceSummary' instead." #-}

-- | The 12-digit account ID or region based on the GroupByKey value.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
accGroupName :: Lens.Lens' AggregateComplianceCount (Core.Maybe Types.GroupName)
accGroupName = Lens.field @"groupName"
{-# DEPRECATED accGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

instance Core.FromJSON AggregateComplianceCount where
  parseJSON =
    Core.withObject "AggregateComplianceCount" Core.$
      \x ->
        AggregateComplianceCount'
          Core.<$> (x Core..:? "ComplianceSummary") Core.<*> (x Core..:? "GroupName")

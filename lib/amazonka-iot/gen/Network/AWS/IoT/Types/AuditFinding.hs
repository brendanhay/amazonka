{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AuditFinding
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.AuditFinding
  ( AuditFinding (..)
  -- * Smart constructor
  , mkAuditFinding
  -- * Lenses
  , afCheckName
  , afFindingId
  , afFindingTime
  , afIsSuppressed
  , afNonCompliantResource
  , afReasonForNonCompliance
  , afReasonForNonComplianceCode
  , afRelatedResources
  , afSeverity
  , afTaskId
  , afTaskStartTime
  ) where

import qualified Network.AWS.IoT.Types.AuditCheckName as Types
import qualified Network.AWS.IoT.Types.AuditFindingSeverity as Types
import qualified Network.AWS.IoT.Types.AuditTaskId as Types
import qualified Network.AWS.IoT.Types.FindingId as Types
import qualified Network.AWS.IoT.Types.NonCompliantResource as Types
import qualified Network.AWS.IoT.Types.ReasonForNonCompliance as Types
import qualified Network.AWS.IoT.Types.ReasonForNonComplianceCode as Types
import qualified Network.AWS.IoT.Types.RelatedResource as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The findings (results) of the audit.
--
-- /See:/ 'mkAuditFinding' smart constructor.
data AuditFinding = AuditFinding'
  { checkName :: Core.Maybe Types.AuditCheckName
    -- ^ The audit check that generated this result.
  , findingId :: Core.Maybe Types.FindingId
    -- ^ A unique identifier for this set of audit findings. This identifier is used to apply mitigation tasks to one or more sets of findings.
  , findingTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The time the result (finding) was discovered.
  , isSuppressed :: Core.Maybe Core.Bool
    -- ^ Indicates whether the audit finding was suppressed or not during reporting. 
  , nonCompliantResource :: Core.Maybe Types.NonCompliantResource
    -- ^ The resource that was found to be noncompliant with the audit check.
  , reasonForNonCompliance :: Core.Maybe Types.ReasonForNonCompliance
    -- ^ The reason the resource was noncompliant.
  , reasonForNonComplianceCode :: Core.Maybe Types.ReasonForNonComplianceCode
    -- ^ A code that indicates the reason that the resource was noncompliant.
  , relatedResources :: Core.Maybe [Types.RelatedResource]
    -- ^ The list of related resources.
  , severity :: Core.Maybe Types.AuditFindingSeverity
    -- ^ The severity of the result (finding).
  , taskId :: Core.Maybe Types.AuditTaskId
    -- ^ The ID of the audit that generated this result (finding).
  , taskStartTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The time the audit started.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'AuditFinding' value with any optional fields omitted.
mkAuditFinding
    :: AuditFinding
mkAuditFinding
  = AuditFinding'{checkName = Core.Nothing, findingId = Core.Nothing,
                  findingTime = Core.Nothing, isSuppressed = Core.Nothing,
                  nonCompliantResource = Core.Nothing,
                  reasonForNonCompliance = Core.Nothing,
                  reasonForNonComplianceCode = Core.Nothing,
                  relatedResources = Core.Nothing, severity = Core.Nothing,
                  taskId = Core.Nothing, taskStartTime = Core.Nothing}

-- | The audit check that generated this result.
--
-- /Note:/ Consider using 'checkName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
afCheckName :: Lens.Lens' AuditFinding (Core.Maybe Types.AuditCheckName)
afCheckName = Lens.field @"checkName"
{-# INLINEABLE afCheckName #-}
{-# DEPRECATED checkName "Use generic-lens or generic-optics with 'checkName' instead"  #-}

-- | A unique identifier for this set of audit findings. This identifier is used to apply mitigation tasks to one or more sets of findings.
--
-- /Note:/ Consider using 'findingId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
afFindingId :: Lens.Lens' AuditFinding (Core.Maybe Types.FindingId)
afFindingId = Lens.field @"findingId"
{-# INLINEABLE afFindingId #-}
{-# DEPRECATED findingId "Use generic-lens or generic-optics with 'findingId' instead"  #-}

-- | The time the result (finding) was discovered.
--
-- /Note:/ Consider using 'findingTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
afFindingTime :: Lens.Lens' AuditFinding (Core.Maybe Core.NominalDiffTime)
afFindingTime = Lens.field @"findingTime"
{-# INLINEABLE afFindingTime #-}
{-# DEPRECATED findingTime "Use generic-lens or generic-optics with 'findingTime' instead"  #-}

-- | Indicates whether the audit finding was suppressed or not during reporting. 
--
-- /Note:/ Consider using 'isSuppressed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
afIsSuppressed :: Lens.Lens' AuditFinding (Core.Maybe Core.Bool)
afIsSuppressed = Lens.field @"isSuppressed"
{-# INLINEABLE afIsSuppressed #-}
{-# DEPRECATED isSuppressed "Use generic-lens or generic-optics with 'isSuppressed' instead"  #-}

-- | The resource that was found to be noncompliant with the audit check.
--
-- /Note:/ Consider using 'nonCompliantResource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
afNonCompliantResource :: Lens.Lens' AuditFinding (Core.Maybe Types.NonCompliantResource)
afNonCompliantResource = Lens.field @"nonCompliantResource"
{-# INLINEABLE afNonCompliantResource #-}
{-# DEPRECATED nonCompliantResource "Use generic-lens or generic-optics with 'nonCompliantResource' instead"  #-}

-- | The reason the resource was noncompliant.
--
-- /Note:/ Consider using 'reasonForNonCompliance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
afReasonForNonCompliance :: Lens.Lens' AuditFinding (Core.Maybe Types.ReasonForNonCompliance)
afReasonForNonCompliance = Lens.field @"reasonForNonCompliance"
{-# INLINEABLE afReasonForNonCompliance #-}
{-# DEPRECATED reasonForNonCompliance "Use generic-lens or generic-optics with 'reasonForNonCompliance' instead"  #-}

-- | A code that indicates the reason that the resource was noncompliant.
--
-- /Note:/ Consider using 'reasonForNonComplianceCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
afReasonForNonComplianceCode :: Lens.Lens' AuditFinding (Core.Maybe Types.ReasonForNonComplianceCode)
afReasonForNonComplianceCode = Lens.field @"reasonForNonComplianceCode"
{-# INLINEABLE afReasonForNonComplianceCode #-}
{-# DEPRECATED reasonForNonComplianceCode "Use generic-lens or generic-optics with 'reasonForNonComplianceCode' instead"  #-}

-- | The list of related resources.
--
-- /Note:/ Consider using 'relatedResources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
afRelatedResources :: Lens.Lens' AuditFinding (Core.Maybe [Types.RelatedResource])
afRelatedResources = Lens.field @"relatedResources"
{-# INLINEABLE afRelatedResources #-}
{-# DEPRECATED relatedResources "Use generic-lens or generic-optics with 'relatedResources' instead"  #-}

-- | The severity of the result (finding).
--
-- /Note:/ Consider using 'severity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
afSeverity :: Lens.Lens' AuditFinding (Core.Maybe Types.AuditFindingSeverity)
afSeverity = Lens.field @"severity"
{-# INLINEABLE afSeverity #-}
{-# DEPRECATED severity "Use generic-lens or generic-optics with 'severity' instead"  #-}

-- | The ID of the audit that generated this result (finding).
--
-- /Note:/ Consider using 'taskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
afTaskId :: Lens.Lens' AuditFinding (Core.Maybe Types.AuditTaskId)
afTaskId = Lens.field @"taskId"
{-# INLINEABLE afTaskId #-}
{-# DEPRECATED taskId "Use generic-lens or generic-optics with 'taskId' instead"  #-}

-- | The time the audit started.
--
-- /Note:/ Consider using 'taskStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
afTaskStartTime :: Lens.Lens' AuditFinding (Core.Maybe Core.NominalDiffTime)
afTaskStartTime = Lens.field @"taskStartTime"
{-# INLINEABLE afTaskStartTime #-}
{-# DEPRECATED taskStartTime "Use generic-lens or generic-optics with 'taskStartTime' instead"  #-}

instance Core.FromJSON AuditFinding where
        parseJSON
          = Core.withObject "AuditFinding" Core.$
              \ x ->
                AuditFinding' Core.<$>
                  (x Core..:? "checkName") Core.<*> x Core..:? "findingId" Core.<*>
                    x Core..:? "findingTime"
                    Core.<*> x Core..:? "isSuppressed"
                    Core.<*> x Core..:? "nonCompliantResource"
                    Core.<*> x Core..:? "reasonForNonCompliance"
                    Core.<*> x Core..:? "reasonForNonComplianceCode"
                    Core.<*> x Core..:? "relatedResources"
                    Core.<*> x Core..:? "severity"
                    Core.<*> x Core..:? "taskId"
                    Core.<*> x Core..:? "taskStartTime"

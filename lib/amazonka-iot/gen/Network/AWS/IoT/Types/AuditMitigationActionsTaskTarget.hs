{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AuditMitigationActionsTaskTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AuditMitigationActionsTaskTarget
  ( AuditMitigationActionsTaskTarget (..),

    -- * Smart constructor
    mkAuditMitigationActionsTaskTarget,

    -- * Lenses
    amattAuditCheckToReasonCodeFilter,
    amattAuditTaskId,
    amattFindingIds,
  )
where

import qualified Network.AWS.IoT.Types.AuditCheckName as Types
import qualified Network.AWS.IoT.Types.AuditTaskId as Types
import qualified Network.AWS.IoT.Types.FindingId as Types
import qualified Network.AWS.IoT.Types.ReasonForNonComplianceCode as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Used in MitigationActionParams, this information identifies the target findings to which the mitigation actions are applied. Only one entry appears.
--
-- /See:/ 'mkAuditMitigationActionsTaskTarget' smart constructor.
data AuditMitigationActionsTaskTarget = AuditMitigationActionsTaskTarget'
  { -- | Specifies a filter in the form of an audit check and set of reason codes that identify the findings from the audit to which the audit mitigation actions task apply.
    auditCheckToReasonCodeFilter :: Core.Maybe (Core.HashMap Types.AuditCheckName (Core.NonEmpty Types.ReasonForNonComplianceCode)),
    -- | If the task will apply a mitigation action to findings from a specific audit, this value uniquely identifies the audit.
    auditTaskId :: Core.Maybe Types.AuditTaskId,
    -- | If the task will apply a mitigation action to one or more listed findings, this value uniquely identifies those findings.
    findingIds :: Core.Maybe (Core.NonEmpty Types.FindingId)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AuditMitigationActionsTaskTarget' value with any optional fields omitted.
mkAuditMitigationActionsTaskTarget ::
  AuditMitigationActionsTaskTarget
mkAuditMitigationActionsTaskTarget =
  AuditMitigationActionsTaskTarget'
    { auditCheckToReasonCodeFilter =
        Core.Nothing,
      auditTaskId = Core.Nothing,
      findingIds = Core.Nothing
    }

-- | Specifies a filter in the form of an audit check and set of reason codes that identify the findings from the audit to which the audit mitigation actions task apply.
--
-- /Note:/ Consider using 'auditCheckToReasonCodeFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amattAuditCheckToReasonCodeFilter :: Lens.Lens' AuditMitigationActionsTaskTarget (Core.Maybe (Core.HashMap Types.AuditCheckName (Core.NonEmpty Types.ReasonForNonComplianceCode)))
amattAuditCheckToReasonCodeFilter = Lens.field @"auditCheckToReasonCodeFilter"
{-# DEPRECATED amattAuditCheckToReasonCodeFilter "Use generic-lens or generic-optics with 'auditCheckToReasonCodeFilter' instead." #-}

-- | If the task will apply a mitigation action to findings from a specific audit, this value uniquely identifies the audit.
--
-- /Note:/ Consider using 'auditTaskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amattAuditTaskId :: Lens.Lens' AuditMitigationActionsTaskTarget (Core.Maybe Types.AuditTaskId)
amattAuditTaskId = Lens.field @"auditTaskId"
{-# DEPRECATED amattAuditTaskId "Use generic-lens or generic-optics with 'auditTaskId' instead." #-}

-- | If the task will apply a mitigation action to one or more listed findings, this value uniquely identifies those findings.
--
-- /Note:/ Consider using 'findingIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amattFindingIds :: Lens.Lens' AuditMitigationActionsTaskTarget (Core.Maybe (Core.NonEmpty Types.FindingId))
amattFindingIds = Lens.field @"findingIds"
{-# DEPRECATED amattFindingIds "Use generic-lens or generic-optics with 'findingIds' instead." #-}

instance Core.FromJSON AuditMitigationActionsTaskTarget where
  toJSON AuditMitigationActionsTaskTarget {..} =
    Core.object
      ( Core.catMaybes
          [ ("auditCheckToReasonCodeFilter" Core..=)
              Core.<$> auditCheckToReasonCodeFilter,
            ("auditTaskId" Core..=) Core.<$> auditTaskId,
            ("findingIds" Core..=) Core.<$> findingIds
          ]
      )

instance Core.FromJSON AuditMitigationActionsTaskTarget where
  parseJSON =
    Core.withObject "AuditMitigationActionsTaskTarget" Core.$
      \x ->
        AuditMitigationActionsTaskTarget'
          Core.<$> (x Core..:? "auditCheckToReasonCodeFilter")
          Core.<*> (x Core..:? "auditTaskId")
          Core.<*> (x Core..:? "findingIds")

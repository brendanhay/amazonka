{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AuditCheckDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AuditCheckDetails
  ( AuditCheckDetails (..),

    -- * Smart constructor
    mkAuditCheckDetails,

    -- * Lenses
    acdCheckCompliant,
    acdCheckRunStatus,
    acdErrorCode,
    acdMessage,
    acdNonCompliantResourcesCount,
    acdSuppressedNonCompliantResourcesCount,
    acdTotalResourcesCount,
  )
where

import qualified Network.AWS.IoT.Types.AuditCheckRunStatus as Types
import qualified Network.AWS.IoT.Types.ErrorCode as Types
import qualified Network.AWS.IoT.Types.ErrorMessage as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about the audit check.
--
-- /See:/ 'mkAuditCheckDetails' smart constructor.
data AuditCheckDetails = AuditCheckDetails'
  { -- | True if the check is complete and found all resources compliant.
    checkCompliant :: Core.Maybe Core.Bool,
    -- | The completion status of this check. One of "IN_PROGRESS", "WAITING_FOR_DATA_COLLECTION", "CANCELED", "COMPLETED_COMPLIANT", "COMPLETED_NON_COMPLIANT", or "FAILED".
    checkRunStatus :: Core.Maybe Types.AuditCheckRunStatus,
    -- | The code of any error encountered when this check is performed during this audit. One of "INSUFFICIENT_PERMISSIONS" or "AUDIT_CHECK_DISABLED".
    errorCode :: Core.Maybe Types.ErrorCode,
    -- | The message associated with any error encountered when this check is performed during this audit.
    message :: Core.Maybe Types.ErrorMessage,
    -- | The number of resources that were found noncompliant during the check.
    nonCompliantResourcesCount :: Core.Maybe Core.Integer,
    -- | Describes how many of the non-compliant resources created during the evaluation of an audit check were marked as suppressed.
    suppressedNonCompliantResourcesCount :: Core.Maybe Core.Integer,
    -- | The number of resources on which the check was performed.
    totalResourcesCount :: Core.Maybe Core.Integer
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AuditCheckDetails' value with any optional fields omitted.
mkAuditCheckDetails ::
  AuditCheckDetails
mkAuditCheckDetails =
  AuditCheckDetails'
    { checkCompliant = Core.Nothing,
      checkRunStatus = Core.Nothing,
      errorCode = Core.Nothing,
      message = Core.Nothing,
      nonCompliantResourcesCount = Core.Nothing,
      suppressedNonCompliantResourcesCount = Core.Nothing,
      totalResourcesCount = Core.Nothing
    }

-- | True if the check is complete and found all resources compliant.
--
-- /Note:/ Consider using 'checkCompliant' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acdCheckCompliant :: Lens.Lens' AuditCheckDetails (Core.Maybe Core.Bool)
acdCheckCompliant = Lens.field @"checkCompliant"
{-# DEPRECATED acdCheckCompliant "Use generic-lens or generic-optics with 'checkCompliant' instead." #-}

-- | The completion status of this check. One of "IN_PROGRESS", "WAITING_FOR_DATA_COLLECTION", "CANCELED", "COMPLETED_COMPLIANT", "COMPLETED_NON_COMPLIANT", or "FAILED".
--
-- /Note:/ Consider using 'checkRunStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acdCheckRunStatus :: Lens.Lens' AuditCheckDetails (Core.Maybe Types.AuditCheckRunStatus)
acdCheckRunStatus = Lens.field @"checkRunStatus"
{-# DEPRECATED acdCheckRunStatus "Use generic-lens or generic-optics with 'checkRunStatus' instead." #-}

-- | The code of any error encountered when this check is performed during this audit. One of "INSUFFICIENT_PERMISSIONS" or "AUDIT_CHECK_DISABLED".
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acdErrorCode :: Lens.Lens' AuditCheckDetails (Core.Maybe Types.ErrorCode)
acdErrorCode = Lens.field @"errorCode"
{-# DEPRECATED acdErrorCode "Use generic-lens or generic-optics with 'errorCode' instead." #-}

-- | The message associated with any error encountered when this check is performed during this audit.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acdMessage :: Lens.Lens' AuditCheckDetails (Core.Maybe Types.ErrorMessage)
acdMessage = Lens.field @"message"
{-# DEPRECATED acdMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | The number of resources that were found noncompliant during the check.
--
-- /Note:/ Consider using 'nonCompliantResourcesCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acdNonCompliantResourcesCount :: Lens.Lens' AuditCheckDetails (Core.Maybe Core.Integer)
acdNonCompliantResourcesCount = Lens.field @"nonCompliantResourcesCount"
{-# DEPRECATED acdNonCompliantResourcesCount "Use generic-lens or generic-optics with 'nonCompliantResourcesCount' instead." #-}

-- | Describes how many of the non-compliant resources created during the evaluation of an audit check were marked as suppressed.
--
-- /Note:/ Consider using 'suppressedNonCompliantResourcesCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acdSuppressedNonCompliantResourcesCount :: Lens.Lens' AuditCheckDetails (Core.Maybe Core.Integer)
acdSuppressedNonCompliantResourcesCount = Lens.field @"suppressedNonCompliantResourcesCount"
{-# DEPRECATED acdSuppressedNonCompliantResourcesCount "Use generic-lens or generic-optics with 'suppressedNonCompliantResourcesCount' instead." #-}

-- | The number of resources on which the check was performed.
--
-- /Note:/ Consider using 'totalResourcesCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acdTotalResourcesCount :: Lens.Lens' AuditCheckDetails (Core.Maybe Core.Integer)
acdTotalResourcesCount = Lens.field @"totalResourcesCount"
{-# DEPRECATED acdTotalResourcesCount "Use generic-lens or generic-optics with 'totalResourcesCount' instead." #-}

instance Core.FromJSON AuditCheckDetails where
  parseJSON =
    Core.withObject "AuditCheckDetails" Core.$
      \x ->
        AuditCheckDetails'
          Core.<$> (x Core..:? "checkCompliant")
          Core.<*> (x Core..:? "checkRunStatus")
          Core.<*> (x Core..:? "errorCode")
          Core.<*> (x Core..:? "message")
          Core.<*> (x Core..:? "nonCompliantResourcesCount")
          Core.<*> (x Core..:? "suppressedNonCompliantResourcesCount")
          Core.<*> (x Core..:? "totalResourcesCount")

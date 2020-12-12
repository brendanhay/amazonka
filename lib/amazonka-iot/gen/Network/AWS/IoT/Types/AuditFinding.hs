{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AuditFinding
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AuditFinding
  ( AuditFinding (..),

    -- * Smart constructor
    mkAuditFinding,

    -- * Lenses
    afIsSuppressed,
    afTaskId,
    afFindingTime,
    afTaskStartTime,
    afReasonForNonComplianceCode,
    afSeverity,
    afRelatedResources,
    afCheckName,
    afNonCompliantResource,
    afReasonForNonCompliance,
    afFindingId,
  )
where

import Network.AWS.IoT.Types.AuditFindingSeverity
import Network.AWS.IoT.Types.NonCompliantResource
import Network.AWS.IoT.Types.RelatedResource
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The findings (results) of the audit.
--
-- /See:/ 'mkAuditFinding' smart constructor.
data AuditFinding = AuditFinding'
  { isSuppressed ::
      Lude.Maybe Lude.Bool,
    taskId :: Lude.Maybe Lude.Text,
    findingTime :: Lude.Maybe Lude.Timestamp,
    taskStartTime :: Lude.Maybe Lude.Timestamp,
    reasonForNonComplianceCode :: Lude.Maybe Lude.Text,
    severity :: Lude.Maybe AuditFindingSeverity,
    relatedResources :: Lude.Maybe [RelatedResource],
    checkName :: Lude.Maybe Lude.Text,
    nonCompliantResource :: Lude.Maybe NonCompliantResource,
    reasonForNonCompliance :: Lude.Maybe Lude.Text,
    findingId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AuditFinding' with the minimum fields required to make a request.
--
-- * 'checkName' - The audit check that generated this result.
-- * 'findingId' - A unique identifier for this set of audit findings. This identifier is used to apply mitigation tasks to one or more sets of findings.
-- * 'findingTime' - The time the result (finding) was discovered.
-- * 'isSuppressed' - Indicates whether the audit finding was suppressed or not during reporting.
-- * 'nonCompliantResource' - The resource that was found to be noncompliant with the audit check.
-- * 'reasonForNonCompliance' - The reason the resource was noncompliant.
-- * 'reasonForNonComplianceCode' - A code that indicates the reason that the resource was noncompliant.
-- * 'relatedResources' - The list of related resources.
-- * 'severity' - The severity of the result (finding).
-- * 'taskId' - The ID of the audit that generated this result (finding).
-- * 'taskStartTime' - The time the audit started.
mkAuditFinding ::
  AuditFinding
mkAuditFinding =
  AuditFinding'
    { isSuppressed = Lude.Nothing,
      taskId = Lude.Nothing,
      findingTime = Lude.Nothing,
      taskStartTime = Lude.Nothing,
      reasonForNonComplianceCode = Lude.Nothing,
      severity = Lude.Nothing,
      relatedResources = Lude.Nothing,
      checkName = Lude.Nothing,
      nonCompliantResource = Lude.Nothing,
      reasonForNonCompliance = Lude.Nothing,
      findingId = Lude.Nothing
    }

-- | Indicates whether the audit finding was suppressed or not during reporting.
--
-- /Note:/ Consider using 'isSuppressed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
afIsSuppressed :: Lens.Lens' AuditFinding (Lude.Maybe Lude.Bool)
afIsSuppressed = Lens.lens (isSuppressed :: AuditFinding -> Lude.Maybe Lude.Bool) (\s a -> s {isSuppressed = a} :: AuditFinding)
{-# DEPRECATED afIsSuppressed "Use generic-lens or generic-optics with 'isSuppressed' instead." #-}

-- | The ID of the audit that generated this result (finding).
--
-- /Note:/ Consider using 'taskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
afTaskId :: Lens.Lens' AuditFinding (Lude.Maybe Lude.Text)
afTaskId = Lens.lens (taskId :: AuditFinding -> Lude.Maybe Lude.Text) (\s a -> s {taskId = a} :: AuditFinding)
{-# DEPRECATED afTaskId "Use generic-lens or generic-optics with 'taskId' instead." #-}

-- | The time the result (finding) was discovered.
--
-- /Note:/ Consider using 'findingTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
afFindingTime :: Lens.Lens' AuditFinding (Lude.Maybe Lude.Timestamp)
afFindingTime = Lens.lens (findingTime :: AuditFinding -> Lude.Maybe Lude.Timestamp) (\s a -> s {findingTime = a} :: AuditFinding)
{-# DEPRECATED afFindingTime "Use generic-lens or generic-optics with 'findingTime' instead." #-}

-- | The time the audit started.
--
-- /Note:/ Consider using 'taskStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
afTaskStartTime :: Lens.Lens' AuditFinding (Lude.Maybe Lude.Timestamp)
afTaskStartTime = Lens.lens (taskStartTime :: AuditFinding -> Lude.Maybe Lude.Timestamp) (\s a -> s {taskStartTime = a} :: AuditFinding)
{-# DEPRECATED afTaskStartTime "Use generic-lens or generic-optics with 'taskStartTime' instead." #-}

-- | A code that indicates the reason that the resource was noncompliant.
--
-- /Note:/ Consider using 'reasonForNonComplianceCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
afReasonForNonComplianceCode :: Lens.Lens' AuditFinding (Lude.Maybe Lude.Text)
afReasonForNonComplianceCode = Lens.lens (reasonForNonComplianceCode :: AuditFinding -> Lude.Maybe Lude.Text) (\s a -> s {reasonForNonComplianceCode = a} :: AuditFinding)
{-# DEPRECATED afReasonForNonComplianceCode "Use generic-lens or generic-optics with 'reasonForNonComplianceCode' instead." #-}

-- | The severity of the result (finding).
--
-- /Note:/ Consider using 'severity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
afSeverity :: Lens.Lens' AuditFinding (Lude.Maybe AuditFindingSeverity)
afSeverity = Lens.lens (severity :: AuditFinding -> Lude.Maybe AuditFindingSeverity) (\s a -> s {severity = a} :: AuditFinding)
{-# DEPRECATED afSeverity "Use generic-lens or generic-optics with 'severity' instead." #-}

-- | The list of related resources.
--
-- /Note:/ Consider using 'relatedResources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
afRelatedResources :: Lens.Lens' AuditFinding (Lude.Maybe [RelatedResource])
afRelatedResources = Lens.lens (relatedResources :: AuditFinding -> Lude.Maybe [RelatedResource]) (\s a -> s {relatedResources = a} :: AuditFinding)
{-# DEPRECATED afRelatedResources "Use generic-lens or generic-optics with 'relatedResources' instead." #-}

-- | The audit check that generated this result.
--
-- /Note:/ Consider using 'checkName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
afCheckName :: Lens.Lens' AuditFinding (Lude.Maybe Lude.Text)
afCheckName = Lens.lens (checkName :: AuditFinding -> Lude.Maybe Lude.Text) (\s a -> s {checkName = a} :: AuditFinding)
{-# DEPRECATED afCheckName "Use generic-lens or generic-optics with 'checkName' instead." #-}

-- | The resource that was found to be noncompliant with the audit check.
--
-- /Note:/ Consider using 'nonCompliantResource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
afNonCompliantResource :: Lens.Lens' AuditFinding (Lude.Maybe NonCompliantResource)
afNonCompliantResource = Lens.lens (nonCompliantResource :: AuditFinding -> Lude.Maybe NonCompliantResource) (\s a -> s {nonCompliantResource = a} :: AuditFinding)
{-# DEPRECATED afNonCompliantResource "Use generic-lens or generic-optics with 'nonCompliantResource' instead." #-}

-- | The reason the resource was noncompliant.
--
-- /Note:/ Consider using 'reasonForNonCompliance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
afReasonForNonCompliance :: Lens.Lens' AuditFinding (Lude.Maybe Lude.Text)
afReasonForNonCompliance = Lens.lens (reasonForNonCompliance :: AuditFinding -> Lude.Maybe Lude.Text) (\s a -> s {reasonForNonCompliance = a} :: AuditFinding)
{-# DEPRECATED afReasonForNonCompliance "Use generic-lens or generic-optics with 'reasonForNonCompliance' instead." #-}

-- | A unique identifier for this set of audit findings. This identifier is used to apply mitigation tasks to one or more sets of findings.
--
-- /Note:/ Consider using 'findingId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
afFindingId :: Lens.Lens' AuditFinding (Lude.Maybe Lude.Text)
afFindingId = Lens.lens (findingId :: AuditFinding -> Lude.Maybe Lude.Text) (\s a -> s {findingId = a} :: AuditFinding)
{-# DEPRECATED afFindingId "Use generic-lens or generic-optics with 'findingId' instead." #-}

instance Lude.FromJSON AuditFinding where
  parseJSON =
    Lude.withObject
      "AuditFinding"
      ( \x ->
          AuditFinding'
            Lude.<$> (x Lude..:? "isSuppressed")
            Lude.<*> (x Lude..:? "taskId")
            Lude.<*> (x Lude..:? "findingTime")
            Lude.<*> (x Lude..:? "taskStartTime")
            Lude.<*> (x Lude..:? "reasonForNonComplianceCode")
            Lude.<*> (x Lude..:? "severity")
            Lude.<*> (x Lude..:? "relatedResources" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "checkName")
            Lude.<*> (x Lude..:? "nonCompliantResource")
            Lude.<*> (x Lude..:? "reasonForNonCompliance")
            Lude.<*> (x Lude..:? "findingId")
      )

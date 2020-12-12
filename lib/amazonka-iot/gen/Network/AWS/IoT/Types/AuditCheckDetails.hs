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
    acdSuppressedNonCompliantResourcesCount,
    acdTotalResourcesCount,
    acdCheckCompliant,
    acdNonCompliantResourcesCount,
    acdErrorCode,
    acdMessage,
    acdCheckRunStatus,
  )
where

import Network.AWS.IoT.Types.AuditCheckRunStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about the audit check.
--
-- /See:/ 'mkAuditCheckDetails' smart constructor.
data AuditCheckDetails = AuditCheckDetails'
  { suppressedNonCompliantResourcesCount ::
      Lude.Maybe Lude.Integer,
    totalResourcesCount :: Lude.Maybe Lude.Integer,
    checkCompliant :: Lude.Maybe Lude.Bool,
    nonCompliantResourcesCount :: Lude.Maybe Lude.Integer,
    errorCode :: Lude.Maybe Lude.Text,
    message :: Lude.Maybe Lude.Text,
    checkRunStatus :: Lude.Maybe AuditCheckRunStatus
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AuditCheckDetails' with the minimum fields required to make a request.
--
-- * 'checkCompliant' - True if the check is complete and found all resources compliant.
-- * 'checkRunStatus' - The completion status of this check. One of "IN_PROGRESS", "WAITING_FOR_DATA_COLLECTION", "CANCELED", "COMPLETED_COMPLIANT", "COMPLETED_NON_COMPLIANT", or "FAILED".
-- * 'errorCode' - The code of any error encountered when this check is performed during this audit. One of "INSUFFICIENT_PERMISSIONS" or "AUDIT_CHECK_DISABLED".
-- * 'message' - The message associated with any error encountered when this check is performed during this audit.
-- * 'nonCompliantResourcesCount' - The number of resources that were found noncompliant during the check.
-- * 'suppressedNonCompliantResourcesCount' - Describes how many of the non-compliant resources created during the evaluation of an audit check were marked as suppressed.
-- * 'totalResourcesCount' - The number of resources on which the check was performed.
mkAuditCheckDetails ::
  AuditCheckDetails
mkAuditCheckDetails =
  AuditCheckDetails'
    { suppressedNonCompliantResourcesCount =
        Lude.Nothing,
      totalResourcesCount = Lude.Nothing,
      checkCompliant = Lude.Nothing,
      nonCompliantResourcesCount = Lude.Nothing,
      errorCode = Lude.Nothing,
      message = Lude.Nothing,
      checkRunStatus = Lude.Nothing
    }

-- | Describes how many of the non-compliant resources created during the evaluation of an audit check were marked as suppressed.
--
-- /Note:/ Consider using 'suppressedNonCompliantResourcesCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acdSuppressedNonCompliantResourcesCount :: Lens.Lens' AuditCheckDetails (Lude.Maybe Lude.Integer)
acdSuppressedNonCompliantResourcesCount = Lens.lens (suppressedNonCompliantResourcesCount :: AuditCheckDetails -> Lude.Maybe Lude.Integer) (\s a -> s {suppressedNonCompliantResourcesCount = a} :: AuditCheckDetails)
{-# DEPRECATED acdSuppressedNonCompliantResourcesCount "Use generic-lens or generic-optics with 'suppressedNonCompliantResourcesCount' instead." #-}

-- | The number of resources on which the check was performed.
--
-- /Note:/ Consider using 'totalResourcesCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acdTotalResourcesCount :: Lens.Lens' AuditCheckDetails (Lude.Maybe Lude.Integer)
acdTotalResourcesCount = Lens.lens (totalResourcesCount :: AuditCheckDetails -> Lude.Maybe Lude.Integer) (\s a -> s {totalResourcesCount = a} :: AuditCheckDetails)
{-# DEPRECATED acdTotalResourcesCount "Use generic-lens or generic-optics with 'totalResourcesCount' instead." #-}

-- | True if the check is complete and found all resources compliant.
--
-- /Note:/ Consider using 'checkCompliant' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acdCheckCompliant :: Lens.Lens' AuditCheckDetails (Lude.Maybe Lude.Bool)
acdCheckCompliant = Lens.lens (checkCompliant :: AuditCheckDetails -> Lude.Maybe Lude.Bool) (\s a -> s {checkCompliant = a} :: AuditCheckDetails)
{-# DEPRECATED acdCheckCompliant "Use generic-lens or generic-optics with 'checkCompliant' instead." #-}

-- | The number of resources that were found noncompliant during the check.
--
-- /Note:/ Consider using 'nonCompliantResourcesCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acdNonCompliantResourcesCount :: Lens.Lens' AuditCheckDetails (Lude.Maybe Lude.Integer)
acdNonCompliantResourcesCount = Lens.lens (nonCompliantResourcesCount :: AuditCheckDetails -> Lude.Maybe Lude.Integer) (\s a -> s {nonCompliantResourcesCount = a} :: AuditCheckDetails)
{-# DEPRECATED acdNonCompliantResourcesCount "Use generic-lens or generic-optics with 'nonCompliantResourcesCount' instead." #-}

-- | The code of any error encountered when this check is performed during this audit. One of "INSUFFICIENT_PERMISSIONS" or "AUDIT_CHECK_DISABLED".
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acdErrorCode :: Lens.Lens' AuditCheckDetails (Lude.Maybe Lude.Text)
acdErrorCode = Lens.lens (errorCode :: AuditCheckDetails -> Lude.Maybe Lude.Text) (\s a -> s {errorCode = a} :: AuditCheckDetails)
{-# DEPRECATED acdErrorCode "Use generic-lens or generic-optics with 'errorCode' instead." #-}

-- | The message associated with any error encountered when this check is performed during this audit.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acdMessage :: Lens.Lens' AuditCheckDetails (Lude.Maybe Lude.Text)
acdMessage = Lens.lens (message :: AuditCheckDetails -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: AuditCheckDetails)
{-# DEPRECATED acdMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | The completion status of this check. One of "IN_PROGRESS", "WAITING_FOR_DATA_COLLECTION", "CANCELED", "COMPLETED_COMPLIANT", "COMPLETED_NON_COMPLIANT", or "FAILED".
--
-- /Note:/ Consider using 'checkRunStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acdCheckRunStatus :: Lens.Lens' AuditCheckDetails (Lude.Maybe AuditCheckRunStatus)
acdCheckRunStatus = Lens.lens (checkRunStatus :: AuditCheckDetails -> Lude.Maybe AuditCheckRunStatus) (\s a -> s {checkRunStatus = a} :: AuditCheckDetails)
{-# DEPRECATED acdCheckRunStatus "Use generic-lens or generic-optics with 'checkRunStatus' instead." #-}

instance Lude.FromJSON AuditCheckDetails where
  parseJSON =
    Lude.withObject
      "AuditCheckDetails"
      ( \x ->
          AuditCheckDetails'
            Lude.<$> (x Lude..:? "suppressedNonCompliantResourcesCount")
            Lude.<*> (x Lude..:? "totalResourcesCount")
            Lude.<*> (x Lude..:? "checkCompliant")
            Lude.<*> (x Lude..:? "nonCompliantResourcesCount")
            Lude.<*> (x Lude..:? "errorCode")
            Lude.<*> (x Lude..:? "message")
            Lude.<*> (x Lude..:? "checkRunStatus")
      )

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.RecipientDsnFields
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.RecipientDsnFields
  ( RecipientDsnFields (..),

    -- * Smart constructor
    mkRecipientDsnFields,

    -- * Lenses
    rdfAction,
    rdfStatus,
    rdfDiagnosticCode,
    rdfExtensionFields,
    rdfFinalRecipient,
    rdfLastAttemptDate,
    rdfRemoteMta,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SES.Types.Address as Types
import qualified Network.AWS.SES.Types.DiagnosticCode as Types
import qualified Network.AWS.SES.Types.DsnAction as Types
import qualified Network.AWS.SES.Types.DsnStatus as Types
import qualified Network.AWS.SES.Types.ExtensionField as Types
import qualified Network.AWS.SES.Types.RemoteMta as Types

-- | Recipient-related information to include in the Delivery Status Notification (DSN) when an email that Amazon SES receives on your behalf bounces.
--
-- For information about receiving email through Amazon SES, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkRecipientDsnFields' smart constructor.
data RecipientDsnFields = RecipientDsnFields'
  { -- | The action performed by the reporting mail transfer agent (MTA) as a result of its attempt to deliver the message to the recipient address. This is required by <https://tools.ietf.org/html/rfc3464 RFC 3464> .
    action :: Types.DsnAction,
    -- | The status code that indicates what went wrong. This is required by <https://tools.ietf.org/html/rfc3464 RFC 3464> .
    status :: Types.DsnStatus,
    -- | An extended explanation of what went wrong; this is usually an SMTP response. See <https://tools.ietf.org/html/rfc3463 RFC 3463> for the correct formatting of this parameter.
    diagnosticCode :: Core.Maybe Types.DiagnosticCode,
    -- | Additional X-headers to include in the DSN.
    extensionFields :: Core.Maybe [Types.ExtensionField],
    -- | The email address that the message was ultimately delivered to. This corresponds to the @Final-Recipient@ in the DSN. If not specified, @FinalRecipient@ will be set to the @Recipient@ specified in the @BouncedRecipientInfo@ structure. Either @FinalRecipient@ or the recipient in @BouncedRecipientInfo@ must be a recipient of the original bounced message.
    finalRecipient :: Core.Maybe Types.Address,
    -- | The time the final delivery attempt was made, in <https://www.ietf.org/rfc/rfc0822.txt RFC 822> date-time format.
    lastAttemptDate :: Core.Maybe Core.UTCTime,
    -- | The MTA to which the remote MTA attempted to deliver the message, formatted as specified in <https://tools.ietf.org/html/rfc3464 RFC 3464> (@mta-name-type; mta-name@ ). This parameter typically applies only to propagating synchronous bounces.
    remoteMta :: Core.Maybe Types.RemoteMta
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'RecipientDsnFields' value with any optional fields omitted.
mkRecipientDsnFields ::
  -- | 'action'
  Types.DsnAction ->
  -- | 'status'
  Types.DsnStatus ->
  RecipientDsnFields
mkRecipientDsnFields action status =
  RecipientDsnFields'
    { action,
      status,
      diagnosticCode = Core.Nothing,
      extensionFields = Core.Nothing,
      finalRecipient = Core.Nothing,
      lastAttemptDate = Core.Nothing,
      remoteMta = Core.Nothing
    }

-- | The action performed by the reporting mail transfer agent (MTA) as a result of its attempt to deliver the message to the recipient address. This is required by <https://tools.ietf.org/html/rfc3464 RFC 3464> .
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdfAction :: Lens.Lens' RecipientDsnFields Types.DsnAction
rdfAction = Lens.field @"action"
{-# DEPRECATED rdfAction "Use generic-lens or generic-optics with 'action' instead." #-}

-- | The status code that indicates what went wrong. This is required by <https://tools.ietf.org/html/rfc3464 RFC 3464> .
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdfStatus :: Lens.Lens' RecipientDsnFields Types.DsnStatus
rdfStatus = Lens.field @"status"
{-# DEPRECATED rdfStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | An extended explanation of what went wrong; this is usually an SMTP response. See <https://tools.ietf.org/html/rfc3463 RFC 3463> for the correct formatting of this parameter.
--
-- /Note:/ Consider using 'diagnosticCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdfDiagnosticCode :: Lens.Lens' RecipientDsnFields (Core.Maybe Types.DiagnosticCode)
rdfDiagnosticCode = Lens.field @"diagnosticCode"
{-# DEPRECATED rdfDiagnosticCode "Use generic-lens or generic-optics with 'diagnosticCode' instead." #-}

-- | Additional X-headers to include in the DSN.
--
-- /Note:/ Consider using 'extensionFields' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdfExtensionFields :: Lens.Lens' RecipientDsnFields (Core.Maybe [Types.ExtensionField])
rdfExtensionFields = Lens.field @"extensionFields"
{-# DEPRECATED rdfExtensionFields "Use generic-lens or generic-optics with 'extensionFields' instead." #-}

-- | The email address that the message was ultimately delivered to. This corresponds to the @Final-Recipient@ in the DSN. If not specified, @FinalRecipient@ will be set to the @Recipient@ specified in the @BouncedRecipientInfo@ structure. Either @FinalRecipient@ or the recipient in @BouncedRecipientInfo@ must be a recipient of the original bounced message.
--
-- /Note:/ Consider using 'finalRecipient' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdfFinalRecipient :: Lens.Lens' RecipientDsnFields (Core.Maybe Types.Address)
rdfFinalRecipient = Lens.field @"finalRecipient"
{-# DEPRECATED rdfFinalRecipient "Use generic-lens or generic-optics with 'finalRecipient' instead." #-}

-- | The time the final delivery attempt was made, in <https://www.ietf.org/rfc/rfc0822.txt RFC 822> date-time format.
--
-- /Note:/ Consider using 'lastAttemptDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdfLastAttemptDate :: Lens.Lens' RecipientDsnFields (Core.Maybe Core.UTCTime)
rdfLastAttemptDate = Lens.field @"lastAttemptDate"
{-# DEPRECATED rdfLastAttemptDate "Use generic-lens or generic-optics with 'lastAttemptDate' instead." #-}

-- | The MTA to which the remote MTA attempted to deliver the message, formatted as specified in <https://tools.ietf.org/html/rfc3464 RFC 3464> (@mta-name-type; mta-name@ ). This parameter typically applies only to propagating synchronous bounces.
--
-- /Note:/ Consider using 'remoteMta' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdfRemoteMta :: Lens.Lens' RecipientDsnFields (Core.Maybe Types.RemoteMta)
rdfRemoteMta = Lens.field @"remoteMta"
{-# DEPRECATED rdfRemoteMta "Use generic-lens or generic-optics with 'remoteMta' instead." #-}

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
    rdfStatus,
    rdfDiagnosticCode,
    rdfAction,
    rdfRemoteMta,
    rdfFinalRecipient,
    rdfExtensionFields,
    rdfLastAttemptDate,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SES.Types.DsnAction
import Network.AWS.SES.Types.ExtensionField

-- | Recipient-related information to include in the Delivery Status Notification (DSN) when an email that Amazon SES receives on your behalf bounces.
--
-- For information about receiving email through Amazon SES, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkRecipientDsnFields' smart constructor.
data RecipientDsnFields = RecipientDsnFields'
  { -- | The status code that indicates what went wrong. This is required by <https://tools.ietf.org/html/rfc3464 RFC 3464> .
    status :: Lude.Text,
    -- | An extended explanation of what went wrong; this is usually an SMTP response. See <https://tools.ietf.org/html/rfc3463 RFC 3463> for the correct formatting of this parameter.
    diagnosticCode :: Lude.Maybe Lude.Text,
    -- | The action performed by the reporting mail transfer agent (MTA) as a result of its attempt to deliver the message to the recipient address. This is required by <https://tools.ietf.org/html/rfc3464 RFC 3464> .
    action :: DsnAction,
    -- | The MTA to which the remote MTA attempted to deliver the message, formatted as specified in <https://tools.ietf.org/html/rfc3464 RFC 3464> (@mta-name-type; mta-name@ ). This parameter typically applies only to propagating synchronous bounces.
    remoteMta :: Lude.Maybe Lude.Text,
    -- | The email address that the message was ultimately delivered to. This corresponds to the @Final-Recipient@ in the DSN. If not specified, @FinalRecipient@ will be set to the @Recipient@ specified in the @BouncedRecipientInfo@ structure. Either @FinalRecipient@ or the recipient in @BouncedRecipientInfo@ must be a recipient of the original bounced message.
    finalRecipient :: Lude.Maybe Lude.Text,
    -- | Additional X-headers to include in the DSN.
    extensionFields :: Lude.Maybe [ExtensionField],
    -- | The time the final delivery attempt was made, in <https://www.ietf.org/rfc/rfc0822.txt RFC 822> date-time format.
    lastAttemptDate :: Lude.Maybe Lude.DateTime
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RecipientDsnFields' with the minimum fields required to make a request.
--
-- * 'status' - The status code that indicates what went wrong. This is required by <https://tools.ietf.org/html/rfc3464 RFC 3464> .
-- * 'diagnosticCode' - An extended explanation of what went wrong; this is usually an SMTP response. See <https://tools.ietf.org/html/rfc3463 RFC 3463> for the correct formatting of this parameter.
-- * 'action' - The action performed by the reporting mail transfer agent (MTA) as a result of its attempt to deliver the message to the recipient address. This is required by <https://tools.ietf.org/html/rfc3464 RFC 3464> .
-- * 'remoteMta' - The MTA to which the remote MTA attempted to deliver the message, formatted as specified in <https://tools.ietf.org/html/rfc3464 RFC 3464> (@mta-name-type; mta-name@ ). This parameter typically applies only to propagating synchronous bounces.
-- * 'finalRecipient' - The email address that the message was ultimately delivered to. This corresponds to the @Final-Recipient@ in the DSN. If not specified, @FinalRecipient@ will be set to the @Recipient@ specified in the @BouncedRecipientInfo@ structure. Either @FinalRecipient@ or the recipient in @BouncedRecipientInfo@ must be a recipient of the original bounced message.
-- * 'extensionFields' - Additional X-headers to include in the DSN.
-- * 'lastAttemptDate' - The time the final delivery attempt was made, in <https://www.ietf.org/rfc/rfc0822.txt RFC 822> date-time format.
mkRecipientDsnFields ::
  -- | 'status'
  Lude.Text ->
  -- | 'action'
  DsnAction ->
  RecipientDsnFields
mkRecipientDsnFields pStatus_ pAction_ =
  RecipientDsnFields'
    { status = pStatus_,
      diagnosticCode = Lude.Nothing,
      action = pAction_,
      remoteMta = Lude.Nothing,
      finalRecipient = Lude.Nothing,
      extensionFields = Lude.Nothing,
      lastAttemptDate = Lude.Nothing
    }

-- | The status code that indicates what went wrong. This is required by <https://tools.ietf.org/html/rfc3464 RFC 3464> .
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdfStatus :: Lens.Lens' RecipientDsnFields Lude.Text
rdfStatus = Lens.lens (status :: RecipientDsnFields -> Lude.Text) (\s a -> s {status = a} :: RecipientDsnFields)
{-# DEPRECATED rdfStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | An extended explanation of what went wrong; this is usually an SMTP response. See <https://tools.ietf.org/html/rfc3463 RFC 3463> for the correct formatting of this parameter.
--
-- /Note:/ Consider using 'diagnosticCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdfDiagnosticCode :: Lens.Lens' RecipientDsnFields (Lude.Maybe Lude.Text)
rdfDiagnosticCode = Lens.lens (diagnosticCode :: RecipientDsnFields -> Lude.Maybe Lude.Text) (\s a -> s {diagnosticCode = a} :: RecipientDsnFields)
{-# DEPRECATED rdfDiagnosticCode "Use generic-lens or generic-optics with 'diagnosticCode' instead." #-}

-- | The action performed by the reporting mail transfer agent (MTA) as a result of its attempt to deliver the message to the recipient address. This is required by <https://tools.ietf.org/html/rfc3464 RFC 3464> .
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdfAction :: Lens.Lens' RecipientDsnFields DsnAction
rdfAction = Lens.lens (action :: RecipientDsnFields -> DsnAction) (\s a -> s {action = a} :: RecipientDsnFields)
{-# DEPRECATED rdfAction "Use generic-lens or generic-optics with 'action' instead." #-}

-- | The MTA to which the remote MTA attempted to deliver the message, formatted as specified in <https://tools.ietf.org/html/rfc3464 RFC 3464> (@mta-name-type; mta-name@ ). This parameter typically applies only to propagating synchronous bounces.
--
-- /Note:/ Consider using 'remoteMta' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdfRemoteMta :: Lens.Lens' RecipientDsnFields (Lude.Maybe Lude.Text)
rdfRemoteMta = Lens.lens (remoteMta :: RecipientDsnFields -> Lude.Maybe Lude.Text) (\s a -> s {remoteMta = a} :: RecipientDsnFields)
{-# DEPRECATED rdfRemoteMta "Use generic-lens or generic-optics with 'remoteMta' instead." #-}

-- | The email address that the message was ultimately delivered to. This corresponds to the @Final-Recipient@ in the DSN. If not specified, @FinalRecipient@ will be set to the @Recipient@ specified in the @BouncedRecipientInfo@ structure. Either @FinalRecipient@ or the recipient in @BouncedRecipientInfo@ must be a recipient of the original bounced message.
--
-- /Note:/ Consider using 'finalRecipient' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdfFinalRecipient :: Lens.Lens' RecipientDsnFields (Lude.Maybe Lude.Text)
rdfFinalRecipient = Lens.lens (finalRecipient :: RecipientDsnFields -> Lude.Maybe Lude.Text) (\s a -> s {finalRecipient = a} :: RecipientDsnFields)
{-# DEPRECATED rdfFinalRecipient "Use generic-lens or generic-optics with 'finalRecipient' instead." #-}

-- | Additional X-headers to include in the DSN.
--
-- /Note:/ Consider using 'extensionFields' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdfExtensionFields :: Lens.Lens' RecipientDsnFields (Lude.Maybe [ExtensionField])
rdfExtensionFields = Lens.lens (extensionFields :: RecipientDsnFields -> Lude.Maybe [ExtensionField]) (\s a -> s {extensionFields = a} :: RecipientDsnFields)
{-# DEPRECATED rdfExtensionFields "Use generic-lens or generic-optics with 'extensionFields' instead." #-}

-- | The time the final delivery attempt was made, in <https://www.ietf.org/rfc/rfc0822.txt RFC 822> date-time format.
--
-- /Note:/ Consider using 'lastAttemptDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdfLastAttemptDate :: Lens.Lens' RecipientDsnFields (Lude.Maybe Lude.DateTime)
rdfLastAttemptDate = Lens.lens (lastAttemptDate :: RecipientDsnFields -> Lude.Maybe Lude.DateTime) (\s a -> s {lastAttemptDate = a} :: RecipientDsnFields)
{-# DEPRECATED rdfLastAttemptDate "Use generic-lens or generic-optics with 'lastAttemptDate' instead." #-}

instance Lude.ToQuery RecipientDsnFields where
  toQuery RecipientDsnFields' {..} =
    Lude.mconcat
      [ "Status" Lude.=: status,
        "DiagnosticCode" Lude.=: diagnosticCode,
        "Action" Lude.=: action,
        "RemoteMta" Lude.=: remoteMta,
        "FinalRecipient" Lude.=: finalRecipient,
        "ExtensionFields"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> extensionFields),
        "LastAttemptDate" Lude.=: lastAttemptDate
      ]

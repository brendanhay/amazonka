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
    rdfDiagnosticCode,
    rdfRemoteMta,
    rdfFinalRecipient,
    rdfExtensionFields,
    rdfLastAttemptDate,
    rdfAction,
    rdfStatus,
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
  { diagnosticCode ::
      Lude.Maybe Lude.Text,
    remoteMta :: Lude.Maybe Lude.Text,
    finalRecipient :: Lude.Maybe Lude.Text,
    extensionFields :: Lude.Maybe [ExtensionField],
    lastAttemptDate :: Lude.Maybe Lude.DateTime,
    action :: DsnAction,
    status :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RecipientDsnFields' with the minimum fields required to make a request.
--
-- * 'action' - The action performed by the reporting mail transfer agent (MTA) as a result of its attempt to deliver the message to the recipient address. This is required by <https://tools.ietf.org/html/rfc3464 RFC 3464> .
-- * 'diagnosticCode' - An extended explanation of what went wrong; this is usually an SMTP response. See <https://tools.ietf.org/html/rfc3463 RFC 3463> for the correct formatting of this parameter.
-- * 'extensionFields' - Additional X-headers to include in the DSN.
-- * 'finalRecipient' - The email address that the message was ultimately delivered to. This corresponds to the @Final-Recipient@ in the DSN. If not specified, @FinalRecipient@ will be set to the @Recipient@ specified in the @BouncedRecipientInfo@ structure. Either @FinalRecipient@ or the recipient in @BouncedRecipientInfo@ must be a recipient of the original bounced message.
-- * 'lastAttemptDate' - The time the final delivery attempt was made, in <https://www.ietf.org/rfc/rfc0822.txt RFC 822> date-time format.
-- * 'remoteMta' - The MTA to which the remote MTA attempted to deliver the message, formatted as specified in <https://tools.ietf.org/html/rfc3464 RFC 3464> (@mta-name-type; mta-name@ ). This parameter typically applies only to propagating synchronous bounces.
-- * 'status' - The status code that indicates what went wrong. This is required by <https://tools.ietf.org/html/rfc3464 RFC 3464> .
mkRecipientDsnFields ::
  -- | 'action'
  DsnAction ->
  -- | 'status'
  Lude.Text ->
  RecipientDsnFields
mkRecipientDsnFields pAction_ pStatus_ =
  RecipientDsnFields'
    { diagnosticCode = Lude.Nothing,
      remoteMta = Lude.Nothing,
      finalRecipient = Lude.Nothing,
      extensionFields = Lude.Nothing,
      lastAttemptDate = Lude.Nothing,
      action = pAction_,
      status = pStatus_
    }

-- | An extended explanation of what went wrong; this is usually an SMTP response. See <https://tools.ietf.org/html/rfc3463 RFC 3463> for the correct formatting of this parameter.
--
-- /Note:/ Consider using 'diagnosticCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdfDiagnosticCode :: Lens.Lens' RecipientDsnFields (Lude.Maybe Lude.Text)
rdfDiagnosticCode = Lens.lens (diagnosticCode :: RecipientDsnFields -> Lude.Maybe Lude.Text) (\s a -> s {diagnosticCode = a} :: RecipientDsnFields)
{-# DEPRECATED rdfDiagnosticCode "Use generic-lens or generic-optics with 'diagnosticCode' instead." #-}

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

-- | The action performed by the reporting mail transfer agent (MTA) as a result of its attempt to deliver the message to the recipient address. This is required by <https://tools.ietf.org/html/rfc3464 RFC 3464> .
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdfAction :: Lens.Lens' RecipientDsnFields DsnAction
rdfAction = Lens.lens (action :: RecipientDsnFields -> DsnAction) (\s a -> s {action = a} :: RecipientDsnFields)
{-# DEPRECATED rdfAction "Use generic-lens or generic-optics with 'action' instead." #-}

-- | The status code that indicates what went wrong. This is required by <https://tools.ietf.org/html/rfc3464 RFC 3464> .
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdfStatus :: Lens.Lens' RecipientDsnFields Lude.Text
rdfStatus = Lens.lens (status :: RecipientDsnFields -> Lude.Text) (\s a -> s {status = a} :: RecipientDsnFields)
{-# DEPRECATED rdfStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Lude.ToQuery RecipientDsnFields where
  toQuery RecipientDsnFields' {..} =
    Lude.mconcat
      [ "DiagnosticCode" Lude.=: diagnosticCode,
        "RemoteMta" Lude.=: remoteMta,
        "FinalRecipient" Lude.=: finalRecipient,
        "ExtensionFields"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> extensionFields),
        "LastAttemptDate" Lude.=: lastAttemptDate,
        "Action" Lude.=: action,
        "Status" Lude.=: status
      ]

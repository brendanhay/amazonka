{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.RecipientDsnFields
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.RecipientDsnFields where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SES.Types.DsnAction
import Network.AWS.SES.Types.ExtensionField

-- | Recipient-related information to include in the Delivery Status
-- Notification (DSN) when an email that Amazon SES receives on your behalf
-- bounces.
--
-- For information about receiving email through Amazon SES, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email.html Amazon SES Developer Guide>.
--
-- /See:/ 'newRecipientDsnFields' smart constructor.
data RecipientDsnFields = RecipientDsnFields'
  { -- | The MTA to which the remote MTA attempted to deliver the message,
    -- formatted as specified in <https://tools.ietf.org/html/rfc3464 RFC 3464>
    -- (@mta-name-type; mta-name@). This parameter typically applies only to
    -- propagating synchronous bounces.
    remoteMta :: Prelude.Maybe Prelude.Text,
    -- | The time the final delivery attempt was made, in
    -- <https://www.ietf.org/rfc/rfc0822.txt RFC 822> date-time format.
    lastAttemptDate :: Prelude.Maybe Prelude.ISO8601,
    -- | Additional X-headers to include in the DSN.
    extensionFields :: Prelude.Maybe [ExtensionField],
    -- | An extended explanation of what went wrong; this is usually an SMTP
    -- response. See <https://tools.ietf.org/html/rfc3463 RFC 3463> for the
    -- correct formatting of this parameter.
    diagnosticCode :: Prelude.Maybe Prelude.Text,
    -- | The email address that the message was ultimately delivered to. This
    -- corresponds to the @Final-Recipient@ in the DSN. If not specified,
    -- @FinalRecipient@ will be set to the @Recipient@ specified in the
    -- @BouncedRecipientInfo@ structure. Either @FinalRecipient@ or the
    -- recipient in @BouncedRecipientInfo@ must be a recipient of the original
    -- bounced message.
    --
    -- Do not prepend the @FinalRecipient@ email address with @rfc 822;@, as
    -- described in <https://tools.ietf.org/html/rfc3798 RFC 3798>.
    finalRecipient :: Prelude.Maybe Prelude.Text,
    -- | The action performed by the reporting mail transfer agent (MTA) as a
    -- result of its attempt to deliver the message to the recipient address.
    -- This is required by <https://tools.ietf.org/html/rfc3464 RFC 3464>.
    action :: DsnAction,
    -- | The status code that indicates what went wrong. This is required by
    -- <https://tools.ietf.org/html/rfc3464 RFC 3464>.
    status :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RecipientDsnFields' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'remoteMta', 'recipientDsnFields_remoteMta' - The MTA to which the remote MTA attempted to deliver the message,
-- formatted as specified in <https://tools.ietf.org/html/rfc3464 RFC 3464>
-- (@mta-name-type; mta-name@). This parameter typically applies only to
-- propagating synchronous bounces.
--
-- 'lastAttemptDate', 'recipientDsnFields_lastAttemptDate' - The time the final delivery attempt was made, in
-- <https://www.ietf.org/rfc/rfc0822.txt RFC 822> date-time format.
--
-- 'extensionFields', 'recipientDsnFields_extensionFields' - Additional X-headers to include in the DSN.
--
-- 'diagnosticCode', 'recipientDsnFields_diagnosticCode' - An extended explanation of what went wrong; this is usually an SMTP
-- response. See <https://tools.ietf.org/html/rfc3463 RFC 3463> for the
-- correct formatting of this parameter.
--
-- 'finalRecipient', 'recipientDsnFields_finalRecipient' - The email address that the message was ultimately delivered to. This
-- corresponds to the @Final-Recipient@ in the DSN. If not specified,
-- @FinalRecipient@ will be set to the @Recipient@ specified in the
-- @BouncedRecipientInfo@ structure. Either @FinalRecipient@ or the
-- recipient in @BouncedRecipientInfo@ must be a recipient of the original
-- bounced message.
--
-- Do not prepend the @FinalRecipient@ email address with @rfc 822;@, as
-- described in <https://tools.ietf.org/html/rfc3798 RFC 3798>.
--
-- 'action', 'recipientDsnFields_action' - The action performed by the reporting mail transfer agent (MTA) as a
-- result of its attempt to deliver the message to the recipient address.
-- This is required by <https://tools.ietf.org/html/rfc3464 RFC 3464>.
--
-- 'status', 'recipientDsnFields_status' - The status code that indicates what went wrong. This is required by
-- <https://tools.ietf.org/html/rfc3464 RFC 3464>.
newRecipientDsnFields ::
  -- | 'action'
  DsnAction ->
  -- | 'status'
  Prelude.Text ->
  RecipientDsnFields
newRecipientDsnFields pAction_ pStatus_ =
  RecipientDsnFields'
    { remoteMta = Prelude.Nothing,
      lastAttemptDate = Prelude.Nothing,
      extensionFields = Prelude.Nothing,
      diagnosticCode = Prelude.Nothing,
      finalRecipient = Prelude.Nothing,
      action = pAction_,
      status = pStatus_
    }

-- | The MTA to which the remote MTA attempted to deliver the message,
-- formatted as specified in <https://tools.ietf.org/html/rfc3464 RFC 3464>
-- (@mta-name-type; mta-name@). This parameter typically applies only to
-- propagating synchronous bounces.
recipientDsnFields_remoteMta :: Lens.Lens' RecipientDsnFields (Prelude.Maybe Prelude.Text)
recipientDsnFields_remoteMta = Lens.lens (\RecipientDsnFields' {remoteMta} -> remoteMta) (\s@RecipientDsnFields' {} a -> s {remoteMta = a} :: RecipientDsnFields)

-- | The time the final delivery attempt was made, in
-- <https://www.ietf.org/rfc/rfc0822.txt RFC 822> date-time format.
recipientDsnFields_lastAttemptDate :: Lens.Lens' RecipientDsnFields (Prelude.Maybe Prelude.UTCTime)
recipientDsnFields_lastAttemptDate = Lens.lens (\RecipientDsnFields' {lastAttemptDate} -> lastAttemptDate) (\s@RecipientDsnFields' {} a -> s {lastAttemptDate = a} :: RecipientDsnFields) Prelude.. Lens.mapping Prelude._Time

-- | Additional X-headers to include in the DSN.
recipientDsnFields_extensionFields :: Lens.Lens' RecipientDsnFields (Prelude.Maybe [ExtensionField])
recipientDsnFields_extensionFields = Lens.lens (\RecipientDsnFields' {extensionFields} -> extensionFields) (\s@RecipientDsnFields' {} a -> s {extensionFields = a} :: RecipientDsnFields) Prelude.. Lens.mapping Prelude._Coerce

-- | An extended explanation of what went wrong; this is usually an SMTP
-- response. See <https://tools.ietf.org/html/rfc3463 RFC 3463> for the
-- correct formatting of this parameter.
recipientDsnFields_diagnosticCode :: Lens.Lens' RecipientDsnFields (Prelude.Maybe Prelude.Text)
recipientDsnFields_diagnosticCode = Lens.lens (\RecipientDsnFields' {diagnosticCode} -> diagnosticCode) (\s@RecipientDsnFields' {} a -> s {diagnosticCode = a} :: RecipientDsnFields)

-- | The email address that the message was ultimately delivered to. This
-- corresponds to the @Final-Recipient@ in the DSN. If not specified,
-- @FinalRecipient@ will be set to the @Recipient@ specified in the
-- @BouncedRecipientInfo@ structure. Either @FinalRecipient@ or the
-- recipient in @BouncedRecipientInfo@ must be a recipient of the original
-- bounced message.
--
-- Do not prepend the @FinalRecipient@ email address with @rfc 822;@, as
-- described in <https://tools.ietf.org/html/rfc3798 RFC 3798>.
recipientDsnFields_finalRecipient :: Lens.Lens' RecipientDsnFields (Prelude.Maybe Prelude.Text)
recipientDsnFields_finalRecipient = Lens.lens (\RecipientDsnFields' {finalRecipient} -> finalRecipient) (\s@RecipientDsnFields' {} a -> s {finalRecipient = a} :: RecipientDsnFields)

-- | The action performed by the reporting mail transfer agent (MTA) as a
-- result of its attempt to deliver the message to the recipient address.
-- This is required by <https://tools.ietf.org/html/rfc3464 RFC 3464>.
recipientDsnFields_action :: Lens.Lens' RecipientDsnFields DsnAction
recipientDsnFields_action = Lens.lens (\RecipientDsnFields' {action} -> action) (\s@RecipientDsnFields' {} a -> s {action = a} :: RecipientDsnFields)

-- | The status code that indicates what went wrong. This is required by
-- <https://tools.ietf.org/html/rfc3464 RFC 3464>.
recipientDsnFields_status :: Lens.Lens' RecipientDsnFields Prelude.Text
recipientDsnFields_status = Lens.lens (\RecipientDsnFields' {status} -> status) (\s@RecipientDsnFields' {} a -> s {status = a} :: RecipientDsnFields)

instance Prelude.Hashable RecipientDsnFields

instance Prelude.NFData RecipientDsnFields

instance Prelude.ToQuery RecipientDsnFields where
  toQuery RecipientDsnFields' {..} =
    Prelude.mconcat
      [ "RemoteMta" Prelude.=: remoteMta,
        "LastAttemptDate" Prelude.=: lastAttemptDate,
        "ExtensionFields"
          Prelude.=: Prelude.toQuery
            ( Prelude.toQueryList "member"
                Prelude.<$> extensionFields
            ),
        "DiagnosticCode" Prelude.=: diagnosticCode,
        "FinalRecipient" Prelude.=: finalRecipient,
        "Action" Prelude.=: action,
        "Status" Prelude.=: status
      ]

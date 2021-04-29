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
-- Module      : Network.AWS.SES.Types.ReceiptRule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.ReceiptRule where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SES.Types.ReceiptAction
import Network.AWS.SES.Types.TlsPolicy

-- | Receipt rules enable you to specify which actions Amazon SES should take
-- when it receives mail on behalf of one or more email addresses or
-- domains that you own.
--
-- Each receipt rule defines a set of email addresses or domains that it
-- applies to. If the email addresses or domains match at least one
-- recipient address of the message, Amazon SES executes all of the receipt
-- rule\'s actions on the message.
--
-- For information about setting up receipt rules, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-receipt-rules.html Amazon SES Developer Guide>.
--
-- /See:/ 'newReceiptRule' smart constructor.
data ReceiptRule = ReceiptRule'
  { -- | Specifies whether Amazon SES should require that incoming email is
    -- delivered over a connection encrypted with Transport Layer Security
    -- (TLS). If this parameter is set to @Require@, Amazon SES will bounce
    -- emails that are not received over TLS. The default is @Optional@.
    tlsPolicy :: Prelude.Maybe TlsPolicy,
    -- | If @true@, the receipt rule is active. The default value is @false@.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | An ordered list of actions to perform on messages that match at least
    -- one of the recipient email addresses or domains specified in the receipt
    -- rule.
    actions :: Prelude.Maybe [ReceiptAction],
    -- | The recipient domains and email addresses that the receipt rule applies
    -- to. If this field is not specified, this rule will match all recipients
    -- under all verified domains.
    recipients :: Prelude.Maybe [Prelude.Text],
    -- | If @true@, then messages that this receipt rule applies to are scanned
    -- for spam and viruses. The default value is @false@.
    scanEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The name of the receipt rule. The name must:
    --
    -- -   This value can only contain ASCII letters (a-z, A-Z), numbers (0-9),
    --     underscores (_), or dashes (-).
    --
    -- -   Start and end with a letter or number.
    --
    -- -   Contain less than 64 characters.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ReceiptRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tlsPolicy', 'receiptRule_tlsPolicy' - Specifies whether Amazon SES should require that incoming email is
-- delivered over a connection encrypted with Transport Layer Security
-- (TLS). If this parameter is set to @Require@, Amazon SES will bounce
-- emails that are not received over TLS. The default is @Optional@.
--
-- 'enabled', 'receiptRule_enabled' - If @true@, the receipt rule is active. The default value is @false@.
--
-- 'actions', 'receiptRule_actions' - An ordered list of actions to perform on messages that match at least
-- one of the recipient email addresses or domains specified in the receipt
-- rule.
--
-- 'recipients', 'receiptRule_recipients' - The recipient domains and email addresses that the receipt rule applies
-- to. If this field is not specified, this rule will match all recipients
-- under all verified domains.
--
-- 'scanEnabled', 'receiptRule_scanEnabled' - If @true@, then messages that this receipt rule applies to are scanned
-- for spam and viruses. The default value is @false@.
--
-- 'name', 'receiptRule_name' - The name of the receipt rule. The name must:
--
-- -   This value can only contain ASCII letters (a-z, A-Z), numbers (0-9),
--     underscores (_), or dashes (-).
--
-- -   Start and end with a letter or number.
--
-- -   Contain less than 64 characters.
newReceiptRule ::
  -- | 'name'
  Prelude.Text ->
  ReceiptRule
newReceiptRule pName_ =
  ReceiptRule'
    { tlsPolicy = Prelude.Nothing,
      enabled = Prelude.Nothing,
      actions = Prelude.Nothing,
      recipients = Prelude.Nothing,
      scanEnabled = Prelude.Nothing,
      name = pName_
    }

-- | Specifies whether Amazon SES should require that incoming email is
-- delivered over a connection encrypted with Transport Layer Security
-- (TLS). If this parameter is set to @Require@, Amazon SES will bounce
-- emails that are not received over TLS. The default is @Optional@.
receiptRule_tlsPolicy :: Lens.Lens' ReceiptRule (Prelude.Maybe TlsPolicy)
receiptRule_tlsPolicy = Lens.lens (\ReceiptRule' {tlsPolicy} -> tlsPolicy) (\s@ReceiptRule' {} a -> s {tlsPolicy = a} :: ReceiptRule)

-- | If @true@, the receipt rule is active. The default value is @false@.
receiptRule_enabled :: Lens.Lens' ReceiptRule (Prelude.Maybe Prelude.Bool)
receiptRule_enabled = Lens.lens (\ReceiptRule' {enabled} -> enabled) (\s@ReceiptRule' {} a -> s {enabled = a} :: ReceiptRule)

-- | An ordered list of actions to perform on messages that match at least
-- one of the recipient email addresses or domains specified in the receipt
-- rule.
receiptRule_actions :: Lens.Lens' ReceiptRule (Prelude.Maybe [ReceiptAction])
receiptRule_actions = Lens.lens (\ReceiptRule' {actions} -> actions) (\s@ReceiptRule' {} a -> s {actions = a} :: ReceiptRule) Prelude.. Lens.mapping Prelude._Coerce

-- | The recipient domains and email addresses that the receipt rule applies
-- to. If this field is not specified, this rule will match all recipients
-- under all verified domains.
receiptRule_recipients :: Lens.Lens' ReceiptRule (Prelude.Maybe [Prelude.Text])
receiptRule_recipients = Lens.lens (\ReceiptRule' {recipients} -> recipients) (\s@ReceiptRule' {} a -> s {recipients = a} :: ReceiptRule) Prelude.. Lens.mapping Prelude._Coerce

-- | If @true@, then messages that this receipt rule applies to are scanned
-- for spam and viruses. The default value is @false@.
receiptRule_scanEnabled :: Lens.Lens' ReceiptRule (Prelude.Maybe Prelude.Bool)
receiptRule_scanEnabled = Lens.lens (\ReceiptRule' {scanEnabled} -> scanEnabled) (\s@ReceiptRule' {} a -> s {scanEnabled = a} :: ReceiptRule)

-- | The name of the receipt rule. The name must:
--
-- -   This value can only contain ASCII letters (a-z, A-Z), numbers (0-9),
--     underscores (_), or dashes (-).
--
-- -   Start and end with a letter or number.
--
-- -   Contain less than 64 characters.
receiptRule_name :: Lens.Lens' ReceiptRule Prelude.Text
receiptRule_name = Lens.lens (\ReceiptRule' {name} -> name) (\s@ReceiptRule' {} a -> s {name = a} :: ReceiptRule)

instance Prelude.FromXML ReceiptRule where
  parseXML x =
    ReceiptRule'
      Prelude.<$> (x Prelude..@? "TlsPolicy")
      Prelude.<*> (x Prelude..@? "Enabled")
      Prelude.<*> ( x Prelude..@? "Actions" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "member")
                  )
      Prelude.<*> ( x Prelude..@? "Recipients"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "member")
                  )
      Prelude.<*> (x Prelude..@? "ScanEnabled")
      Prelude.<*> (x Prelude..@ "Name")

instance Prelude.Hashable ReceiptRule

instance Prelude.NFData ReceiptRule

instance Prelude.ToQuery ReceiptRule where
  toQuery ReceiptRule' {..} =
    Prelude.mconcat
      [ "TlsPolicy" Prelude.=: tlsPolicy,
        "Enabled" Prelude.=: enabled,
        "Actions"
          Prelude.=: Prelude.toQuery
            (Prelude.toQueryList "member" Prelude.<$> actions),
        "Recipients"
          Prelude.=: Prelude.toQuery
            ( Prelude.toQueryList "member"
                Prelude.<$> recipients
            ),
        "ScanEnabled" Prelude.=: scanEnabled,
        "Name" Prelude.=: name
      ]

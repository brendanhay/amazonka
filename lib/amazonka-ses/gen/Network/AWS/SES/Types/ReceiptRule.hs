{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.ReceiptRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.ReceiptRule
  ( ReceiptRule (..),

    -- * Smart constructor
    mkReceiptRule,

    -- * Lenses
    rrScanEnabled,
    rrEnabled,
    rrActions,
    rrRecipients,
    rrTLSPolicy,
    rrName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SES.Types.ReceiptAction
import Network.AWS.SES.Types.TLSPolicy

-- | Receipt rules enable you to specify which actions Amazon SES should take when it receives mail on behalf of one or more email addresses or domains that you own.
--
-- Each receipt rule defines a set of email addresses or domains that it applies to. If the email addresses or domains match at least one recipient address of the message, Amazon SES executes all of the receipt rule's actions on the message.
-- For information about setting up receipt rules, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-receipt-rules.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkReceiptRule' smart constructor.
data ReceiptRule = ReceiptRule'
  { scanEnabled ::
      Lude.Maybe Lude.Bool,
    enabled :: Lude.Maybe Lude.Bool,
    actions :: Lude.Maybe [ReceiptAction],
    recipients :: Lude.Maybe [Lude.Text],
    tlsPolicy :: Lude.Maybe TLSPolicy,
    name :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReceiptRule' with the minimum fields required to make a request.
--
-- * 'actions' - An ordered list of actions to perform on messages that match at least one of the recipient email addresses or domains specified in the receipt rule.
-- * 'enabled' - If @true@ , the receipt rule is active. The default value is @false@ .
-- * 'name' - The name of the receipt rule. The name must:
--
--
--     * This value can only contain ASCII letters (a-z, A-Z), numbers (0-9), underscores (_), or dashes (-).
--
--
--     * Start and end with a letter or number.
--
--
--     * Contain less than 64 characters.
--
--
-- * 'recipients' - The recipient domains and email addresses that the receipt rule applies to. If this field is not specified, this rule will match all recipients under all verified domains.
-- * 'scanEnabled' - If @true@ , then messages that this receipt rule applies to are scanned for spam and viruses. The default value is @false@ .
-- * 'tlsPolicy' - Specifies whether Amazon SES should require that incoming email is delivered over a connection encrypted with Transport Layer Security (TLS). If this parameter is set to @Require@ , Amazon SES will bounce emails that are not received over TLS. The default is @Optional@ .
mkReceiptRule ::
  -- | 'name'
  Lude.Text ->
  ReceiptRule
mkReceiptRule pName_ =
  ReceiptRule'
    { scanEnabled = Lude.Nothing,
      enabled = Lude.Nothing,
      actions = Lude.Nothing,
      recipients = Lude.Nothing,
      tlsPolicy = Lude.Nothing,
      name = pName_
    }

-- | If @true@ , then messages that this receipt rule applies to are scanned for spam and viruses. The default value is @false@ .
--
-- /Note:/ Consider using 'scanEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrScanEnabled :: Lens.Lens' ReceiptRule (Lude.Maybe Lude.Bool)
rrScanEnabled = Lens.lens (scanEnabled :: ReceiptRule -> Lude.Maybe Lude.Bool) (\s a -> s {scanEnabled = a} :: ReceiptRule)
{-# DEPRECATED rrScanEnabled "Use generic-lens or generic-optics with 'scanEnabled' instead." #-}

-- | If @true@ , the receipt rule is active. The default value is @false@ .
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrEnabled :: Lens.Lens' ReceiptRule (Lude.Maybe Lude.Bool)
rrEnabled = Lens.lens (enabled :: ReceiptRule -> Lude.Maybe Lude.Bool) (\s a -> s {enabled = a} :: ReceiptRule)
{-# DEPRECATED rrEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | An ordered list of actions to perform on messages that match at least one of the recipient email addresses or domains specified in the receipt rule.
--
-- /Note:/ Consider using 'actions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrActions :: Lens.Lens' ReceiptRule (Lude.Maybe [ReceiptAction])
rrActions = Lens.lens (actions :: ReceiptRule -> Lude.Maybe [ReceiptAction]) (\s a -> s {actions = a} :: ReceiptRule)
{-# DEPRECATED rrActions "Use generic-lens or generic-optics with 'actions' instead." #-}

-- | The recipient domains and email addresses that the receipt rule applies to. If this field is not specified, this rule will match all recipients under all verified domains.
--
-- /Note:/ Consider using 'recipients' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrRecipients :: Lens.Lens' ReceiptRule (Lude.Maybe [Lude.Text])
rrRecipients = Lens.lens (recipients :: ReceiptRule -> Lude.Maybe [Lude.Text]) (\s a -> s {recipients = a} :: ReceiptRule)
{-# DEPRECATED rrRecipients "Use generic-lens or generic-optics with 'recipients' instead." #-}

-- | Specifies whether Amazon SES should require that incoming email is delivered over a connection encrypted with Transport Layer Security (TLS). If this parameter is set to @Require@ , Amazon SES will bounce emails that are not received over TLS. The default is @Optional@ .
--
-- /Note:/ Consider using 'tlsPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrTLSPolicy :: Lens.Lens' ReceiptRule (Lude.Maybe TLSPolicy)
rrTLSPolicy = Lens.lens (tlsPolicy :: ReceiptRule -> Lude.Maybe TLSPolicy) (\s a -> s {tlsPolicy = a} :: ReceiptRule)
{-# DEPRECATED rrTLSPolicy "Use generic-lens or generic-optics with 'tlsPolicy' instead." #-}

-- | The name of the receipt rule. The name must:
--
--
--     * This value can only contain ASCII letters (a-z, A-Z), numbers (0-9), underscores (_), or dashes (-).
--
--
--     * Start and end with a letter or number.
--
--
--     * Contain less than 64 characters.
--
--
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrName :: Lens.Lens' ReceiptRule Lude.Text
rrName = Lens.lens (name :: ReceiptRule -> Lude.Text) (\s a -> s {name = a} :: ReceiptRule)
{-# DEPRECATED rrName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromXML ReceiptRule where
  parseXML x =
    ReceiptRule'
      Lude.<$> (x Lude..@? "ScanEnabled")
      Lude.<*> (x Lude..@? "Enabled")
      Lude.<*> ( x Lude..@? "Actions" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> ( x Lude..@? "Recipients" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> (x Lude..@? "TlsPolicy")
      Lude.<*> (x Lude..@ "Name")

instance Lude.ToQuery ReceiptRule where
  toQuery ReceiptRule' {..} =
    Lude.mconcat
      [ "ScanEnabled" Lude.=: scanEnabled,
        "Enabled" Lude.=: enabled,
        "Actions"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> actions),
        "Recipients"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> recipients),
        "TlsPolicy" Lude.=: tlsPolicy,
        "Name" Lude.=: name
      ]

{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.ReceiptRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SES.Types.ReceiptRule
  ( ReceiptRule (..)
  -- * Smart constructor
  , mkReceiptRule
  -- * Lenses
  , rrName
  , rrActions
  , rrEnabled
  , rrRecipients
  , rrScanEnabled
  , rrTlsPolicy
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SES.Types.ReceiptAction as Types
import qualified Network.AWS.SES.Types.ReceiptRuleName as Types
import qualified Network.AWS.SES.Types.Recipient as Types
import qualified Network.AWS.SES.Types.TlsPolicy as Types

-- | Receipt rules enable you to specify which actions Amazon SES should take when it receives mail on behalf of one or more email addresses or domains that you own.
--
-- Each receipt rule defines a set of email addresses or domains that it applies to. If the email addresses or domains match at least one recipient address of the message, Amazon SES executes all of the receipt rule's actions on the message.
-- For information about setting up receipt rules, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-receipt-rules.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkReceiptRule' smart constructor.
data ReceiptRule = ReceiptRule'
  { name :: Types.ReceiptRuleName
    -- ^ The name of the receipt rule. The name must:
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
  , actions :: Core.Maybe [Types.ReceiptAction]
    -- ^ An ordered list of actions to perform on messages that match at least one of the recipient email addresses or domains specified in the receipt rule.
  , enabled :: Core.Maybe Core.Bool
    -- ^ If @true@ , the receipt rule is active. The default value is @false@ .
  , recipients :: Core.Maybe [Types.Recipient]
    -- ^ The recipient domains and email addresses that the receipt rule applies to. If this field is not specified, this rule will match all recipients under all verified domains.
  , scanEnabled :: Core.Maybe Core.Bool
    -- ^ If @true@ , then messages that this receipt rule applies to are scanned for spam and viruses. The default value is @false@ .
  , tlsPolicy :: Core.Maybe Types.TlsPolicy
    -- ^ Specifies whether Amazon SES should require that incoming email is delivered over a connection encrypted with Transport Layer Security (TLS). If this parameter is set to @Require@ , Amazon SES will bounce emails that are not received over TLS. The default is @Optional@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReceiptRule' value with any optional fields omitted.
mkReceiptRule
    :: Types.ReceiptRuleName -- ^ 'name'
    -> ReceiptRule
mkReceiptRule name
  = ReceiptRule'{name, actions = Core.Nothing,
                 enabled = Core.Nothing, recipients = Core.Nothing,
                 scanEnabled = Core.Nothing, tlsPolicy = Core.Nothing}

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
rrName :: Lens.Lens' ReceiptRule Types.ReceiptRuleName
rrName = Lens.field @"name"
{-# INLINEABLE rrName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | An ordered list of actions to perform on messages that match at least one of the recipient email addresses or domains specified in the receipt rule.
--
-- /Note:/ Consider using 'actions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrActions :: Lens.Lens' ReceiptRule (Core.Maybe [Types.ReceiptAction])
rrActions = Lens.field @"actions"
{-# INLINEABLE rrActions #-}
{-# DEPRECATED actions "Use generic-lens or generic-optics with 'actions' instead"  #-}

-- | If @true@ , the receipt rule is active. The default value is @false@ .
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrEnabled :: Lens.Lens' ReceiptRule (Core.Maybe Core.Bool)
rrEnabled = Lens.field @"enabled"
{-# INLINEABLE rrEnabled #-}
{-# DEPRECATED enabled "Use generic-lens or generic-optics with 'enabled' instead"  #-}

-- | The recipient domains and email addresses that the receipt rule applies to. If this field is not specified, this rule will match all recipients under all verified domains.
--
-- /Note:/ Consider using 'recipients' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrRecipients :: Lens.Lens' ReceiptRule (Core.Maybe [Types.Recipient])
rrRecipients = Lens.field @"recipients"
{-# INLINEABLE rrRecipients #-}
{-# DEPRECATED recipients "Use generic-lens or generic-optics with 'recipients' instead"  #-}

-- | If @true@ , then messages that this receipt rule applies to are scanned for spam and viruses. The default value is @false@ .
--
-- /Note:/ Consider using 'scanEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrScanEnabled :: Lens.Lens' ReceiptRule (Core.Maybe Core.Bool)
rrScanEnabled = Lens.field @"scanEnabled"
{-# INLINEABLE rrScanEnabled #-}
{-# DEPRECATED scanEnabled "Use generic-lens or generic-optics with 'scanEnabled' instead"  #-}

-- | Specifies whether Amazon SES should require that incoming email is delivered over a connection encrypted with Transport Layer Security (TLS). If this parameter is set to @Require@ , Amazon SES will bounce emails that are not received over TLS. The default is @Optional@ .
--
-- /Note:/ Consider using 'tlsPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrTlsPolicy :: Lens.Lens' ReceiptRule (Core.Maybe Types.TlsPolicy)
rrTlsPolicy = Lens.field @"tlsPolicy"
{-# INLINEABLE rrTlsPolicy #-}
{-# DEPRECATED tlsPolicy "Use generic-lens or generic-optics with 'tlsPolicy' instead"  #-}

instance Core.ToQuery ReceiptRule where
        toQuery ReceiptRule{..}
          = Core.toQueryPair "Name" name Core.<>
              Core.toQueryPair "Actions"
                (Core.maybe Core.mempty (Core.toQueryList "member") actions)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Enabled") enabled
              Core.<>
              Core.toQueryPair "Recipients"
                (Core.maybe Core.mempty (Core.toQueryList "member") recipients)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ScanEnabled") scanEnabled
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "TlsPolicy") tlsPolicy

instance Core.FromXML ReceiptRule where
        parseXML x
          = ReceiptRule' Core.<$>
              (x Core..@ "Name") Core.<*>
                x Core..@? "Actions" Core..<@> Core.parseXMLList "member"
                Core.<*> x Core..@? "Enabled"
                Core.<*>
                x Core..@? "Recipients" Core..<@> Core.parseXMLList "member"
                Core.<*> x Core..@? "ScanEnabled"
                Core.<*> x Core..@? "TlsPolicy"

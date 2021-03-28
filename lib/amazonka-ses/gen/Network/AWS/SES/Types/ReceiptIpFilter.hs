{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.ReceiptIpFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SES.Types.ReceiptIpFilter
  ( ReceiptIpFilter (..)
  -- * Smart constructor
  , mkReceiptIpFilter
  -- * Lenses
  , rifPolicy
  , rifCidr
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SES.Types.Cidr as Types
import qualified Network.AWS.SES.Types.ReceiptFilterPolicy as Types

-- | A receipt IP address filter enables you to specify whether to accept or reject mail originating from an IP address or range of IP addresses.
--
-- For information about setting up IP address filters, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-ip-filters.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkReceiptIpFilter' smart constructor.
data ReceiptIpFilter = ReceiptIpFilter'
  { policy :: Types.ReceiptFilterPolicy
    -- ^ Indicates whether to block or allow incoming mail from the specified IP addresses.
  , cidr :: Types.Cidr
    -- ^ A single IP address or a range of IP addresses that you want to block or allow, specified in Classless Inter-Domain Routing (CIDR) notation. An example of a single email address is 10.0.0.1. An example of a range of IP addresses is 10.0.0.1/24. For more information about CIDR notation, see <https://tools.ietf.org/html/rfc2317 RFC 2317> .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReceiptIpFilter' value with any optional fields omitted.
mkReceiptIpFilter
    :: Types.ReceiptFilterPolicy -- ^ 'policy'
    -> Types.Cidr -- ^ 'cidr'
    -> ReceiptIpFilter
mkReceiptIpFilter policy cidr = ReceiptIpFilter'{policy, cidr}

-- | Indicates whether to block or allow incoming mail from the specified IP addresses.
--
-- /Note:/ Consider using 'policy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rifPolicy :: Lens.Lens' ReceiptIpFilter Types.ReceiptFilterPolicy
rifPolicy = Lens.field @"policy"
{-# INLINEABLE rifPolicy #-}
{-# DEPRECATED policy "Use generic-lens or generic-optics with 'policy' instead"  #-}

-- | A single IP address or a range of IP addresses that you want to block or allow, specified in Classless Inter-Domain Routing (CIDR) notation. An example of a single email address is 10.0.0.1. An example of a range of IP addresses is 10.0.0.1/24. For more information about CIDR notation, see <https://tools.ietf.org/html/rfc2317 RFC 2317> .
--
-- /Note:/ Consider using 'cidr' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rifCidr :: Lens.Lens' ReceiptIpFilter Types.Cidr
rifCidr = Lens.field @"cidr"
{-# INLINEABLE rifCidr #-}
{-# DEPRECATED cidr "Use generic-lens or generic-optics with 'cidr' instead"  #-}

instance Core.ToQuery ReceiptIpFilter where
        toQuery ReceiptIpFilter{..}
          = Core.toQueryPair "Policy" policy Core.<>
              Core.toQueryPair "Cidr" cidr

instance Core.FromXML ReceiptIpFilter where
        parseXML x
          = ReceiptIpFilter' Core.<$>
              (x Core..@ "Policy") Core.<*> x Core..@ "Cidr"

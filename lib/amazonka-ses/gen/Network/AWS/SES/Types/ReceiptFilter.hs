{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.ReceiptFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SES.Types.ReceiptFilter
  ( ReceiptFilter (..)
  -- * Smart constructor
  , mkReceiptFilter
  -- * Lenses
  , rfName
  , rfIpFilter
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SES.Types.ReceiptFilterName as Types
import qualified Network.AWS.SES.Types.ReceiptIpFilter as Types

-- | A receipt IP address filter enables you to specify whether to accept or reject mail originating from an IP address or range of IP addresses.
--
-- For information about setting up IP address filters, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-ip-filters.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkReceiptFilter' smart constructor.
data ReceiptFilter = ReceiptFilter'
  { name :: Types.ReceiptFilterName
    -- ^ The name of the IP address filter. The name must:
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
  , ipFilter :: Types.ReceiptIpFilter
    -- ^ A structure that provides the IP addresses to block or allow, and whether to block or allow incoming mail from them.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReceiptFilter' value with any optional fields omitted.
mkReceiptFilter
    :: Types.ReceiptFilterName -- ^ 'name'
    -> Types.ReceiptIpFilter -- ^ 'ipFilter'
    -> ReceiptFilter
mkReceiptFilter name ipFilter = ReceiptFilter'{name, ipFilter}

-- | The name of the IP address filter. The name must:
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
rfName :: Lens.Lens' ReceiptFilter Types.ReceiptFilterName
rfName = Lens.field @"name"
{-# INLINEABLE rfName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | A structure that provides the IP addresses to block or allow, and whether to block or allow incoming mail from them.
--
-- /Note:/ Consider using 'ipFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfIpFilter :: Lens.Lens' ReceiptFilter Types.ReceiptIpFilter
rfIpFilter = Lens.field @"ipFilter"
{-# INLINEABLE rfIpFilter #-}
{-# DEPRECATED ipFilter "Use generic-lens or generic-optics with 'ipFilter' instead"  #-}

instance Core.ToQuery ReceiptFilter where
        toQuery ReceiptFilter{..}
          = Core.toQueryPair "Name" name Core.<>
              Core.toQueryPair "IpFilter" ipFilter

instance Core.FromXML ReceiptFilter where
        parseXML x
          = ReceiptFilter' Core.<$>
              (x Core..@ "Name") Core.<*> x Core..@ "IpFilter"

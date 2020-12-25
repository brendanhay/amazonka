{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Types.IPSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAFRegional.Types.IPSet
  ( IPSet (..),

    -- * Smart constructor
    mkIPSet,

    -- * Lenses
    ipsIPSetId,
    ipsIPSetDescriptors,
    ipsName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WAFRegional.Types.IPSetDescriptor as Types
import qualified Network.AWS.WAFRegional.Types.ResourceId as Types
import qualified Network.AWS.WAFRegional.Types.ResourceName as Types

-- | Contains one or more IP addresses or blocks of IP addresses specified in Classless Inter-Domain Routing (CIDR) notation. AWS WAF supports IPv4 address ranges: /8 and any range between /16 through /32. AWS WAF supports IPv6 address ranges: /24, /32, /48, /56, /64, and /128.
--
-- To specify an individual IP address, you specify the four-part IP address followed by a @/32@ , for example, 192.0.2.0/32. To block a range of IP addresses, you can specify /8 or any range between /16 through /32 (for IPv4) or /24, /32, /48, /56, /64, or /128 (for IPv6). For more information about CIDR notation, see the Wikipedia entry <https://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing Classless Inter-Domain Routing> .
--
-- /See:/ 'mkIPSet' smart constructor.
data IPSet = IPSet'
  { -- | The @IPSetId@ for an @IPSet@ . You use @IPSetId@ to get information about an @IPSet@ (see 'GetIPSet' ), update an @IPSet@ (see 'UpdateIPSet' ), insert an @IPSet@ into a @Rule@ or delete one from a @Rule@ (see 'UpdateRule' ), and delete an @IPSet@ from AWS WAF (see 'DeleteIPSet' ).
    --
    -- @IPSetId@ is returned by 'CreateIPSet' and by 'ListIPSets' .
    iPSetId :: Types.ResourceId,
    -- | The IP address type (@IPV4@ or @IPV6@ ) and the IP address range (in CIDR notation) that web requests originate from. If the @WebACL@ is associated with a CloudFront distribution and the viewer did not use an HTTP proxy or a load balancer to send the request, this is the value of the c-ip field in the CloudFront access logs.
    iPSetDescriptors :: [Types.IPSetDescriptor],
    -- | A friendly name or description of the 'IPSet' . You can't change the name of an @IPSet@ after you create it.
    name :: Core.Maybe Types.ResourceName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'IPSet' value with any optional fields omitted.
mkIPSet ::
  -- | 'iPSetId'
  Types.ResourceId ->
  IPSet
mkIPSet iPSetId =
  IPSet'
    { iPSetId,
      iPSetDescriptors = Core.mempty,
      name = Core.Nothing
    }

-- | The @IPSetId@ for an @IPSet@ . You use @IPSetId@ to get information about an @IPSet@ (see 'GetIPSet' ), update an @IPSet@ (see 'UpdateIPSet' ), insert an @IPSet@ into a @Rule@ or delete one from a @Rule@ (see 'UpdateRule' ), and delete an @IPSet@ from AWS WAF (see 'DeleteIPSet' ).
--
-- @IPSetId@ is returned by 'CreateIPSet' and by 'ListIPSets' .
--
-- /Note:/ Consider using 'iPSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipsIPSetId :: Lens.Lens' IPSet Types.ResourceId
ipsIPSetId = Lens.field @"iPSetId"
{-# DEPRECATED ipsIPSetId "Use generic-lens or generic-optics with 'iPSetId' instead." #-}

-- | The IP address type (@IPV4@ or @IPV6@ ) and the IP address range (in CIDR notation) that web requests originate from. If the @WebACL@ is associated with a CloudFront distribution and the viewer did not use an HTTP proxy or a load balancer to send the request, this is the value of the c-ip field in the CloudFront access logs.
--
-- /Note:/ Consider using 'iPSetDescriptors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipsIPSetDescriptors :: Lens.Lens' IPSet [Types.IPSetDescriptor]
ipsIPSetDescriptors = Lens.field @"iPSetDescriptors"
{-# DEPRECATED ipsIPSetDescriptors "Use generic-lens or generic-optics with 'iPSetDescriptors' instead." #-}

-- | A friendly name or description of the 'IPSet' . You can't change the name of an @IPSet@ after you create it.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipsName :: Lens.Lens' IPSet (Core.Maybe Types.ResourceName)
ipsName = Lens.field @"name"
{-# DEPRECATED ipsName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON IPSet where
  parseJSON =
    Core.withObject "IPSet" Core.$
      \x ->
        IPSet'
          Core.<$> (x Core..: "IPSetId")
          Core.<*> (x Core..:? "IPSetDescriptors" Core..!= Core.mempty)
          Core.<*> (x Core..:? "Name")

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
    isName,
    isIPSetDescriptors,
    isIPSetId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.WAFRegional.Types.IPSetDescriptor

-- | Contains one or more IP addresses or blocks of IP addresses specified in Classless Inter-Domain Routing (CIDR) notation. AWS WAF supports IPv4 address ranges: /8 and any range between /16 through /32. AWS WAF supports IPv6 address ranges: /24, /32, /48, /56, /64, and /128.
--
-- To specify an individual IP address, you specify the four-part IP address followed by a @/32@ , for example, 192.0.2.0/32. To block a range of IP addresses, you can specify /8 or any range between /16 through /32 (for IPv4) or /24, /32, /48, /56, /64, or /128 (for IPv6). For more information about CIDR notation, see the Wikipedia entry <https://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing Classless Inter-Domain Routing> .
--
-- /See:/ 'mkIPSet' smart constructor.
data IPSet = IPSet'
  { -- | A friendly name or description of the 'IPSet' . You can't change the name of an @IPSet@ after you create it.
    name :: Lude.Maybe Lude.Text,
    -- | The IP address type (@IPV4@ or @IPV6@ ) and the IP address range (in CIDR notation) that web requests originate from. If the @WebACL@ is associated with a CloudFront distribution and the viewer did not use an HTTP proxy or a load balancer to send the request, this is the value of the c-ip field in the CloudFront access logs.
    ipSetDescriptors :: [IPSetDescriptor],
    -- | The @IPSetId@ for an @IPSet@ . You use @IPSetId@ to get information about an @IPSet@ (see 'GetIPSet' ), update an @IPSet@ (see 'UpdateIPSet' ), insert an @IPSet@ into a @Rule@ or delete one from a @Rule@ (see 'UpdateRule' ), and delete an @IPSet@ from AWS WAF (see 'DeleteIPSet' ).
    --
    -- @IPSetId@ is returned by 'CreateIPSet' and by 'ListIPSets' .
    ipSetId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'IPSet' with the minimum fields required to make a request.
--
-- * 'name' - A friendly name or description of the 'IPSet' . You can't change the name of an @IPSet@ after you create it.
-- * 'ipSetDescriptors' - The IP address type (@IPV4@ or @IPV6@ ) and the IP address range (in CIDR notation) that web requests originate from. If the @WebACL@ is associated with a CloudFront distribution and the viewer did not use an HTTP proxy or a load balancer to send the request, this is the value of the c-ip field in the CloudFront access logs.
-- * 'ipSetId' - The @IPSetId@ for an @IPSet@ . You use @IPSetId@ to get information about an @IPSet@ (see 'GetIPSet' ), update an @IPSet@ (see 'UpdateIPSet' ), insert an @IPSet@ into a @Rule@ or delete one from a @Rule@ (see 'UpdateRule' ), and delete an @IPSet@ from AWS WAF (see 'DeleteIPSet' ).
--
-- @IPSetId@ is returned by 'CreateIPSet' and by 'ListIPSets' .
mkIPSet ::
  -- | 'ipSetId'
  Lude.Text ->
  IPSet
mkIPSet pIPSetId_ =
  IPSet'
    { name = Lude.Nothing,
      ipSetDescriptors = Lude.mempty,
      ipSetId = pIPSetId_
    }

-- | A friendly name or description of the 'IPSet' . You can't change the name of an @IPSet@ after you create it.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isName :: Lens.Lens' IPSet (Lude.Maybe Lude.Text)
isName = Lens.lens (name :: IPSet -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: IPSet)
{-# DEPRECATED isName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The IP address type (@IPV4@ or @IPV6@ ) and the IP address range (in CIDR notation) that web requests originate from. If the @WebACL@ is associated with a CloudFront distribution and the viewer did not use an HTTP proxy or a load balancer to send the request, this is the value of the c-ip field in the CloudFront access logs.
--
-- /Note:/ Consider using 'ipSetDescriptors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isIPSetDescriptors :: Lens.Lens' IPSet [IPSetDescriptor]
isIPSetDescriptors = Lens.lens (ipSetDescriptors :: IPSet -> [IPSetDescriptor]) (\s a -> s {ipSetDescriptors = a} :: IPSet)
{-# DEPRECATED isIPSetDescriptors "Use generic-lens or generic-optics with 'ipSetDescriptors' instead." #-}

-- | The @IPSetId@ for an @IPSet@ . You use @IPSetId@ to get information about an @IPSet@ (see 'GetIPSet' ), update an @IPSet@ (see 'UpdateIPSet' ), insert an @IPSet@ into a @Rule@ or delete one from a @Rule@ (see 'UpdateRule' ), and delete an @IPSet@ from AWS WAF (see 'DeleteIPSet' ).
--
-- @IPSetId@ is returned by 'CreateIPSet' and by 'ListIPSets' .
--
-- /Note:/ Consider using 'ipSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isIPSetId :: Lens.Lens' IPSet Lude.Text
isIPSetId = Lens.lens (ipSetId :: IPSet -> Lude.Text) (\s a -> s {ipSetId = a} :: IPSet)
{-# DEPRECATED isIPSetId "Use generic-lens or generic-optics with 'ipSetId' instead." #-}

instance Lude.FromJSON IPSet where
  parseJSON =
    Lude.withObject
      "IPSet"
      ( \x ->
          IPSet'
            Lude.<$> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "IPSetDescriptors" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..: "IPSetId")
      )

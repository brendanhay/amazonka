{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.GetCheckerIPRanges
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- /Important:/ @GetCheckerIpRanges@ still works, but we recommend that you download ip-ranges.json, which includes IP address ranges for all AWS services. For more information, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/route-53-ip-addresses.html IP Address Ranges of Amazon Route 53 Servers> in the /Amazon Route 53 Developer Guide/ .
module Network.AWS.Route53.GetCheckerIPRanges
  ( -- * Creating a request
    GetCheckerIPRanges (..),
    mkGetCheckerIPRanges,

    -- * Destructuring the response
    GetCheckerIPRangesResponse (..),
    mkGetCheckerIPRangesResponse,

    -- ** Response lenses
    gcirrsCheckerIPRanges,
    gcirrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53.Types

-- | Empty request.
--
-- /See:/ 'mkGetCheckerIPRanges' smart constructor.
data GetCheckerIPRanges = GetCheckerIPRanges'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetCheckerIPRanges' with the minimum fields required to make a request.
mkGetCheckerIPRanges ::
  GetCheckerIPRanges
mkGetCheckerIPRanges = GetCheckerIPRanges'

instance Lude.AWSRequest GetCheckerIPRanges where
  type Rs GetCheckerIPRanges = GetCheckerIPRangesResponse
  request = Req.get route53Service
  response =
    Res.receiveXML
      ( \s h x ->
          GetCheckerIPRangesResponse'
            Lude.<$> ( x Lude..@? "CheckerIpRanges" Lude..!@ Lude.mempty
                         Lude.>>= Lude.parseXMLList "member"
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetCheckerIPRanges where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetCheckerIPRanges where
  toPath = Lude.const "/2013-04-01/checkeripranges"

instance Lude.ToQuery GetCheckerIPRanges where
  toQuery = Lude.const Lude.mempty

-- | A complex type that contains the @CheckerIpRanges@ element.
--
-- /See:/ 'mkGetCheckerIPRangesResponse' smart constructor.
data GetCheckerIPRangesResponse = GetCheckerIPRangesResponse'
  { -- | A complex type that contains sorted list of IP ranges in CIDR format for Amazon Route 53 health checkers.
    checkerIPRanges :: [Lude.Text],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetCheckerIPRangesResponse' with the minimum fields required to make a request.
--
-- * 'checkerIPRanges' - A complex type that contains sorted list of IP ranges in CIDR format for Amazon Route 53 health checkers.
-- * 'responseStatus' - The response status code.
mkGetCheckerIPRangesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetCheckerIPRangesResponse
mkGetCheckerIPRangesResponse pResponseStatus_ =
  GetCheckerIPRangesResponse'
    { checkerIPRanges = Lude.mempty,
      responseStatus = pResponseStatus_
    }

-- | A complex type that contains sorted list of IP ranges in CIDR format for Amazon Route 53 health checkers.
--
-- /Note:/ Consider using 'checkerIPRanges' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcirrsCheckerIPRanges :: Lens.Lens' GetCheckerIPRangesResponse [Lude.Text]
gcirrsCheckerIPRanges = Lens.lens (checkerIPRanges :: GetCheckerIPRangesResponse -> [Lude.Text]) (\s a -> s {checkerIPRanges = a} :: GetCheckerIPRangesResponse)
{-# DEPRECATED gcirrsCheckerIPRanges "Use generic-lens or generic-optics with 'checkerIPRanges' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcirrsResponseStatus :: Lens.Lens' GetCheckerIPRangesResponse Lude.Int
gcirrsResponseStatus = Lens.lens (responseStatus :: GetCheckerIPRangesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetCheckerIPRangesResponse)
{-# DEPRECATED gcirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

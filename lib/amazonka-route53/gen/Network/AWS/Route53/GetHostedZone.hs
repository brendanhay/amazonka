{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.GetHostedZone
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a specified hosted zone including the four name servers assigned to the hosted zone.
module Network.AWS.Route53.GetHostedZone
  ( -- * Creating a request
    GetHostedZone (..),
    mkGetHostedZone,

    -- ** Request lenses
    ghzId,

    -- * Destructuring the response
    GetHostedZoneResponse (..),
    mkGetHostedZoneResponse,

    -- ** Response lenses
    ghzrsVPCs,
    ghzrsDelegationSet,
    ghzrsResponseStatus,
    ghzrsHostedZone,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53.Types

-- | A request to get information about a specified hosted zone.
--
-- /See:/ 'mkGetHostedZone' smart constructor.
newtype GetHostedZone = GetHostedZone' {id :: ResourceId}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetHostedZone' with the minimum fields required to make a request.
--
-- * 'id' - The ID of the hosted zone that you want to get information about.
mkGetHostedZone ::
  -- | 'id'
  ResourceId ->
  GetHostedZone
mkGetHostedZone pId_ = GetHostedZone' {id = pId_}

-- | The ID of the hosted zone that you want to get information about.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghzId :: Lens.Lens' GetHostedZone ResourceId
ghzId = Lens.lens (id :: GetHostedZone -> ResourceId) (\s a -> s {id = a} :: GetHostedZone)
{-# DEPRECATED ghzId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest GetHostedZone where
  type Rs GetHostedZone = GetHostedZoneResponse
  request = Req.get route53Service
  response =
    Res.receiveXML
      ( \s h x ->
          GetHostedZoneResponse'
            Lude.<$> ( x Lude..@? "VPCs" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLNonEmpty "VPC")
                     )
            Lude.<*> (x Lude..@? "DelegationSet")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..@ "HostedZone")
      )

instance Lude.ToHeaders GetHostedZone where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetHostedZone where
  toPath GetHostedZone' {..} =
    Lude.mconcat ["/2013-04-01/hostedzone/", Lude.toBS id]

instance Lude.ToQuery GetHostedZone where
  toQuery = Lude.const Lude.mempty

-- | A complex type that contain the response to a @GetHostedZone@ request.
--
-- /See:/ 'mkGetHostedZoneResponse' smart constructor.
data GetHostedZoneResponse = GetHostedZoneResponse'
  { vpcs ::
      Lude.Maybe (Lude.NonEmpty VPC),
    delegationSet :: Lude.Maybe DelegationSet,
    responseStatus :: Lude.Int,
    hostedZone :: HostedZone
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetHostedZoneResponse' with the minimum fields required to make a request.
--
-- * 'delegationSet' - A complex type that lists the Amazon Route 53 name servers for the specified hosted zone.
-- * 'hostedZone' - A complex type that contains general information about the specified hosted zone.
-- * 'responseStatus' - The response status code.
-- * 'vpcs' - A complex type that contains information about the VPCs that are associated with the specified hosted zone.
mkGetHostedZoneResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'hostedZone'
  HostedZone ->
  GetHostedZoneResponse
mkGetHostedZoneResponse pResponseStatus_ pHostedZone_ =
  GetHostedZoneResponse'
    { vpcs = Lude.Nothing,
      delegationSet = Lude.Nothing,
      responseStatus = pResponseStatus_,
      hostedZone = pHostedZone_
    }

-- | A complex type that contains information about the VPCs that are associated with the specified hosted zone.
--
-- /Note:/ Consider using 'vpcs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghzrsVPCs :: Lens.Lens' GetHostedZoneResponse (Lude.Maybe (Lude.NonEmpty VPC))
ghzrsVPCs = Lens.lens (vpcs :: GetHostedZoneResponse -> Lude.Maybe (Lude.NonEmpty VPC)) (\s a -> s {vpcs = a} :: GetHostedZoneResponse)
{-# DEPRECATED ghzrsVPCs "Use generic-lens or generic-optics with 'vpcs' instead." #-}

-- | A complex type that lists the Amazon Route 53 name servers for the specified hosted zone.
--
-- /Note:/ Consider using 'delegationSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghzrsDelegationSet :: Lens.Lens' GetHostedZoneResponse (Lude.Maybe DelegationSet)
ghzrsDelegationSet = Lens.lens (delegationSet :: GetHostedZoneResponse -> Lude.Maybe DelegationSet) (\s a -> s {delegationSet = a} :: GetHostedZoneResponse)
{-# DEPRECATED ghzrsDelegationSet "Use generic-lens or generic-optics with 'delegationSet' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghzrsResponseStatus :: Lens.Lens' GetHostedZoneResponse Lude.Int
ghzrsResponseStatus = Lens.lens (responseStatus :: GetHostedZoneResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetHostedZoneResponse)
{-# DEPRECATED ghzrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | A complex type that contains general information about the specified hosted zone.
--
-- /Note:/ Consider using 'hostedZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghzrsHostedZone :: Lens.Lens' GetHostedZoneResponse HostedZone
ghzrsHostedZone = Lens.lens (hostedZone :: GetHostedZoneResponse -> HostedZone) (\s a -> s {hostedZone = a} :: GetHostedZoneResponse)
{-# DEPRECATED ghzrsHostedZone "Use generic-lens or generic-optics with 'hostedZone' instead." #-}

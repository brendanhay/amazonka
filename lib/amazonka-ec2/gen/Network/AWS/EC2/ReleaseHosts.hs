{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ReleaseHosts
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- When you no longer want to use an On-Demand Dedicated Host it can be released. On-Demand billing is stopped and the host goes into @released@ state. The host ID of Dedicated Hosts that have been released can no longer be specified in another request, for example, to modify the host. You must stop or terminate all instances on a host before it can be released.
--
-- When Dedicated Hosts are released, it may take some time for them to stop counting toward your limit and you may receive capacity errors when trying to allocate new Dedicated Hosts. Wait a few minutes and then try again.
-- Released hosts still appear in a 'DescribeHosts' response.
module Network.AWS.EC2.ReleaseHosts
  ( -- * Creating a request
    ReleaseHosts (..),
    mkReleaseHosts,

    -- ** Request lenses
    rhHostIds,

    -- * Destructuring the response
    ReleaseHostsResponse (..),
    mkReleaseHostsResponse,

    -- ** Response lenses
    rhrsUnsuccessful,
    rhrsSuccessful,
    rhrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkReleaseHosts' smart constructor.
newtype ReleaseHosts = ReleaseHosts'
  { -- | The IDs of the Dedicated Hosts to release.
    hostIds :: [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReleaseHosts' with the minimum fields required to make a request.
--
-- * 'hostIds' - The IDs of the Dedicated Hosts to release.
mkReleaseHosts ::
  ReleaseHosts
mkReleaseHosts = ReleaseHosts' {hostIds = Lude.mempty}

-- | The IDs of the Dedicated Hosts to release.
--
-- /Note:/ Consider using 'hostIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rhHostIds :: Lens.Lens' ReleaseHosts [Lude.Text]
rhHostIds = Lens.lens (hostIds :: ReleaseHosts -> [Lude.Text]) (\s a -> s {hostIds = a} :: ReleaseHosts)
{-# DEPRECATED rhHostIds "Use generic-lens or generic-optics with 'hostIds' instead." #-}

instance Lude.AWSRequest ReleaseHosts where
  type Rs ReleaseHosts = ReleaseHostsResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          ReleaseHostsResponse'
            Lude.<$> ( x Lude..@? "unsuccessful" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> ( x Lude..@? "successful" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ReleaseHosts where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ReleaseHosts where
  toPath = Lude.const "/"

instance Lude.ToQuery ReleaseHosts where
  toQuery ReleaseHosts' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ReleaseHosts" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQueryList "HostId" hostIds
      ]

-- | /See:/ 'mkReleaseHostsResponse' smart constructor.
data ReleaseHostsResponse = ReleaseHostsResponse'
  { -- | The IDs of the Dedicated Hosts that could not be released, including an error message.
    unsuccessful :: Lude.Maybe [UnsuccessfulItem],
    -- | The IDs of the Dedicated Hosts that were successfully released.
    successful :: Lude.Maybe [Lude.Text],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReleaseHostsResponse' with the minimum fields required to make a request.
--
-- * 'unsuccessful' - The IDs of the Dedicated Hosts that could not be released, including an error message.
-- * 'successful' - The IDs of the Dedicated Hosts that were successfully released.
-- * 'responseStatus' - The response status code.
mkReleaseHostsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ReleaseHostsResponse
mkReleaseHostsResponse pResponseStatus_ =
  ReleaseHostsResponse'
    { unsuccessful = Lude.Nothing,
      successful = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The IDs of the Dedicated Hosts that could not be released, including an error message.
--
-- /Note:/ Consider using 'unsuccessful' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rhrsUnsuccessful :: Lens.Lens' ReleaseHostsResponse (Lude.Maybe [UnsuccessfulItem])
rhrsUnsuccessful = Lens.lens (unsuccessful :: ReleaseHostsResponse -> Lude.Maybe [UnsuccessfulItem]) (\s a -> s {unsuccessful = a} :: ReleaseHostsResponse)
{-# DEPRECATED rhrsUnsuccessful "Use generic-lens or generic-optics with 'unsuccessful' instead." #-}

-- | The IDs of the Dedicated Hosts that were successfully released.
--
-- /Note:/ Consider using 'successful' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rhrsSuccessful :: Lens.Lens' ReleaseHostsResponse (Lude.Maybe [Lude.Text])
rhrsSuccessful = Lens.lens (successful :: ReleaseHostsResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {successful = a} :: ReleaseHostsResponse)
{-# DEPRECATED rhrsSuccessful "Use generic-lens or generic-optics with 'successful' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rhrsResponseStatus :: Lens.Lens' ReleaseHostsResponse Lude.Int
rhrsResponseStatus = Lens.lens (responseStatus :: ReleaseHostsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ReleaseHostsResponse)
{-# DEPRECATED rhrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

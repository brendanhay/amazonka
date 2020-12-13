{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ModifyTrafficMirrorFilterNetworkServices
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows or restricts mirroring network services.
--
-- By default, Amazon DNS network services are not eligible for Traffic Mirror. Use @AddNetworkServices@ to add network services to a Traffic Mirror filter. When a network service is added to the Traffic Mirror filter, all traffic related to that network service will be mirrored. When you no longer want to mirror network services, use @RemoveNetworkServices@ to remove the network services from the Traffic Mirror filter.
-- For information about filter rule properties, see <https://docs.aws.amazon.com/vpc/latest/mirroring/traffic-mirroring-considerations.html Network Services> in the /Traffic Mirroring User Guide / .
module Network.AWS.EC2.ModifyTrafficMirrorFilterNetworkServices
  ( -- * Creating a request
    ModifyTrafficMirrorFilterNetworkServices (..),
    mkModifyTrafficMirrorFilterNetworkServices,

    -- ** Request lenses
    mtmfnsTrafficMirrorFilterId,
    mtmfnsAddNetworkServices,
    mtmfnsRemoveNetworkServices,
    mtmfnsDryRun,

    -- * Destructuring the response
    ModifyTrafficMirrorFilterNetworkServicesResponse (..),
    mkModifyTrafficMirrorFilterNetworkServicesResponse,

    -- ** Response lenses
    mtmfnsrsTrafficMirrorFilter,
    mtmfnsrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkModifyTrafficMirrorFilterNetworkServices' smart constructor.
data ModifyTrafficMirrorFilterNetworkServices = ModifyTrafficMirrorFilterNetworkServices'
  { -- | The ID of the Traffic Mirror filter.
    trafficMirrorFilterId :: Lude.Text,
    -- | The network service, for example Amazon DNS, that you want to mirror.
    addNetworkServices :: Lude.Maybe [TrafficMirrorNetworkService],
    -- | The network service, for example Amazon DNS, that you no longer want to mirror.
    removeNetworkServices :: Lude.Maybe [TrafficMirrorNetworkService],
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyTrafficMirrorFilterNetworkServices' with the minimum fields required to make a request.
--
-- * 'trafficMirrorFilterId' - The ID of the Traffic Mirror filter.
-- * 'addNetworkServices' - The network service, for example Amazon DNS, that you want to mirror.
-- * 'removeNetworkServices' - The network service, for example Amazon DNS, that you no longer want to mirror.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkModifyTrafficMirrorFilterNetworkServices ::
  -- | 'trafficMirrorFilterId'
  Lude.Text ->
  ModifyTrafficMirrorFilterNetworkServices
mkModifyTrafficMirrorFilterNetworkServices pTrafficMirrorFilterId_ =
  ModifyTrafficMirrorFilterNetworkServices'
    { trafficMirrorFilterId =
        pTrafficMirrorFilterId_,
      addNetworkServices = Lude.Nothing,
      removeNetworkServices = Lude.Nothing,
      dryRun = Lude.Nothing
    }

-- | The ID of the Traffic Mirror filter.
--
-- /Note:/ Consider using 'trafficMirrorFilterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtmfnsTrafficMirrorFilterId :: Lens.Lens' ModifyTrafficMirrorFilterNetworkServices Lude.Text
mtmfnsTrafficMirrorFilterId = Lens.lens (trafficMirrorFilterId :: ModifyTrafficMirrorFilterNetworkServices -> Lude.Text) (\s a -> s {trafficMirrorFilterId = a} :: ModifyTrafficMirrorFilterNetworkServices)
{-# DEPRECATED mtmfnsTrafficMirrorFilterId "Use generic-lens or generic-optics with 'trafficMirrorFilterId' instead." #-}

-- | The network service, for example Amazon DNS, that you want to mirror.
--
-- /Note:/ Consider using 'addNetworkServices' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtmfnsAddNetworkServices :: Lens.Lens' ModifyTrafficMirrorFilterNetworkServices (Lude.Maybe [TrafficMirrorNetworkService])
mtmfnsAddNetworkServices = Lens.lens (addNetworkServices :: ModifyTrafficMirrorFilterNetworkServices -> Lude.Maybe [TrafficMirrorNetworkService]) (\s a -> s {addNetworkServices = a} :: ModifyTrafficMirrorFilterNetworkServices)
{-# DEPRECATED mtmfnsAddNetworkServices "Use generic-lens or generic-optics with 'addNetworkServices' instead." #-}

-- | The network service, for example Amazon DNS, that you no longer want to mirror.
--
-- /Note:/ Consider using 'removeNetworkServices' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtmfnsRemoveNetworkServices :: Lens.Lens' ModifyTrafficMirrorFilterNetworkServices (Lude.Maybe [TrafficMirrorNetworkService])
mtmfnsRemoveNetworkServices = Lens.lens (removeNetworkServices :: ModifyTrafficMirrorFilterNetworkServices -> Lude.Maybe [TrafficMirrorNetworkService]) (\s a -> s {removeNetworkServices = a} :: ModifyTrafficMirrorFilterNetworkServices)
{-# DEPRECATED mtmfnsRemoveNetworkServices "Use generic-lens or generic-optics with 'removeNetworkServices' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtmfnsDryRun :: Lens.Lens' ModifyTrafficMirrorFilterNetworkServices (Lude.Maybe Lude.Bool)
mtmfnsDryRun = Lens.lens (dryRun :: ModifyTrafficMirrorFilterNetworkServices -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: ModifyTrafficMirrorFilterNetworkServices)
{-# DEPRECATED mtmfnsDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest ModifyTrafficMirrorFilterNetworkServices where
  type
    Rs ModifyTrafficMirrorFilterNetworkServices =
      ModifyTrafficMirrorFilterNetworkServicesResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          ModifyTrafficMirrorFilterNetworkServicesResponse'
            Lude.<$> (x Lude..@? "trafficMirrorFilter")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ModifyTrafficMirrorFilterNetworkServices where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ModifyTrafficMirrorFilterNetworkServices where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifyTrafficMirrorFilterNetworkServices where
  toQuery ModifyTrafficMirrorFilterNetworkServices' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("ModifyTrafficMirrorFilterNetworkServices" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "TrafficMirrorFilterId" Lude.=: trafficMirrorFilterId,
        Lude.toQuery
          (Lude.toQueryList "AddNetworkService" Lude.<$> addNetworkServices),
        Lude.toQuery
          ( Lude.toQueryList "RemoveNetworkService"
              Lude.<$> removeNetworkServices
          ),
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkModifyTrafficMirrorFilterNetworkServicesResponse' smart constructor.
data ModifyTrafficMirrorFilterNetworkServicesResponse = ModifyTrafficMirrorFilterNetworkServicesResponse'
  { -- | The Traffic Mirror filter that the network service is associated with.
    trafficMirrorFilter :: Lude.Maybe TrafficMirrorFilter,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyTrafficMirrorFilterNetworkServicesResponse' with the minimum fields required to make a request.
--
-- * 'trafficMirrorFilter' - The Traffic Mirror filter that the network service is associated with.
-- * 'responseStatus' - The response status code.
mkModifyTrafficMirrorFilterNetworkServicesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ModifyTrafficMirrorFilterNetworkServicesResponse
mkModifyTrafficMirrorFilterNetworkServicesResponse pResponseStatus_ =
  ModifyTrafficMirrorFilterNetworkServicesResponse'
    { trafficMirrorFilter =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Traffic Mirror filter that the network service is associated with.
--
-- /Note:/ Consider using 'trafficMirrorFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtmfnsrsTrafficMirrorFilter :: Lens.Lens' ModifyTrafficMirrorFilterNetworkServicesResponse (Lude.Maybe TrafficMirrorFilter)
mtmfnsrsTrafficMirrorFilter = Lens.lens (trafficMirrorFilter :: ModifyTrafficMirrorFilterNetworkServicesResponse -> Lude.Maybe TrafficMirrorFilter) (\s a -> s {trafficMirrorFilter = a} :: ModifyTrafficMirrorFilterNetworkServicesResponse)
{-# DEPRECATED mtmfnsrsTrafficMirrorFilter "Use generic-lens or generic-optics with 'trafficMirrorFilter' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtmfnsrsResponseStatus :: Lens.Lens' ModifyTrafficMirrorFilterNetworkServicesResponse Lude.Int
mtmfnsrsResponseStatus = Lens.lens (responseStatus :: ModifyTrafficMirrorFilterNetworkServicesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ModifyTrafficMirrorFilterNetworkServicesResponse)
{-# DEPRECATED mtmfnsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

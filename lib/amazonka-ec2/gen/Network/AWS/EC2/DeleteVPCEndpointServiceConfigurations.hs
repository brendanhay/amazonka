{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteVPCEndpointServiceConfigurations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes one or more VPC endpoint service configurations in your account. Before you delete the endpoint service configuration, you must reject any @Available@ or @PendingAcceptance@ interface endpoint connections that are attached to the service.
module Network.AWS.EC2.DeleteVPCEndpointServiceConfigurations
  ( -- * Creating a request
    DeleteVPCEndpointServiceConfigurations (..),
    mkDeleteVPCEndpointServiceConfigurations,

    -- ** Request lenses
    dvpcescServiceIds,
    dvpcescDryRun,

    -- * Destructuring the response
    DeleteVPCEndpointServiceConfigurationsResponse (..),
    mkDeleteVPCEndpointServiceConfigurationsResponse,

    -- ** Response lenses
    dvpcescrsUnsuccessful,
    dvpcescrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteVPCEndpointServiceConfigurations' smart constructor.
data DeleteVPCEndpointServiceConfigurations = DeleteVPCEndpointServiceConfigurations'
  { -- | The IDs of one or more services.
    serviceIds :: [Lude.Text],
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteVPCEndpointServiceConfigurations' with the minimum fields required to make a request.
--
-- * 'serviceIds' - The IDs of one or more services.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkDeleteVPCEndpointServiceConfigurations ::
  DeleteVPCEndpointServiceConfigurations
mkDeleteVPCEndpointServiceConfigurations =
  DeleteVPCEndpointServiceConfigurations'
    { serviceIds = Lude.mempty,
      dryRun = Lude.Nothing
    }

-- | The IDs of one or more services.
--
-- /Note:/ Consider using 'serviceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvpcescServiceIds :: Lens.Lens' DeleteVPCEndpointServiceConfigurations [Lude.Text]
dvpcescServiceIds = Lens.lens (serviceIds :: DeleteVPCEndpointServiceConfigurations -> [Lude.Text]) (\s a -> s {serviceIds = a} :: DeleteVPCEndpointServiceConfigurations)
{-# DEPRECATED dvpcescServiceIds "Use generic-lens or generic-optics with 'serviceIds' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvpcescDryRun :: Lens.Lens' DeleteVPCEndpointServiceConfigurations (Lude.Maybe Lude.Bool)
dvpcescDryRun = Lens.lens (dryRun :: DeleteVPCEndpointServiceConfigurations -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DeleteVPCEndpointServiceConfigurations)
{-# DEPRECATED dvpcescDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest DeleteVPCEndpointServiceConfigurations where
  type
    Rs DeleteVPCEndpointServiceConfigurations =
      DeleteVPCEndpointServiceConfigurationsResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DeleteVPCEndpointServiceConfigurationsResponse'
            Lude.<$> ( x Lude..@? "unsuccessful" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteVPCEndpointServiceConfigurations where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteVPCEndpointServiceConfigurations where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteVPCEndpointServiceConfigurations where
  toQuery DeleteVPCEndpointServiceConfigurations' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DeleteVpcEndpointServiceConfigurations" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQueryList "ServiceId" serviceIds,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkDeleteVPCEndpointServiceConfigurationsResponse' smart constructor.
data DeleteVPCEndpointServiceConfigurationsResponse = DeleteVPCEndpointServiceConfigurationsResponse'
  { -- | Information about the service configurations that were not deleted, if applicable.
    unsuccessful :: Lude.Maybe [UnsuccessfulItem],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteVPCEndpointServiceConfigurationsResponse' with the minimum fields required to make a request.
--
-- * 'unsuccessful' - Information about the service configurations that were not deleted, if applicable.
-- * 'responseStatus' - The response status code.
mkDeleteVPCEndpointServiceConfigurationsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteVPCEndpointServiceConfigurationsResponse
mkDeleteVPCEndpointServiceConfigurationsResponse pResponseStatus_ =
  DeleteVPCEndpointServiceConfigurationsResponse'
    { unsuccessful =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the service configurations that were not deleted, if applicable.
--
-- /Note:/ Consider using 'unsuccessful' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvpcescrsUnsuccessful :: Lens.Lens' DeleteVPCEndpointServiceConfigurationsResponse (Lude.Maybe [UnsuccessfulItem])
dvpcescrsUnsuccessful = Lens.lens (unsuccessful :: DeleteVPCEndpointServiceConfigurationsResponse -> Lude.Maybe [UnsuccessfulItem]) (\s a -> s {unsuccessful = a} :: DeleteVPCEndpointServiceConfigurationsResponse)
{-# DEPRECATED dvpcescrsUnsuccessful "Use generic-lens or generic-optics with 'unsuccessful' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvpcescrsResponseStatus :: Lens.Lens' DeleteVPCEndpointServiceConfigurationsResponse Lude.Int
dvpcescrsResponseStatus = Lens.lens (responseStatus :: DeleteVPCEndpointServiceConfigurationsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteVPCEndpointServiceConfigurationsResponse)
{-# DEPRECATED dvpcescrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

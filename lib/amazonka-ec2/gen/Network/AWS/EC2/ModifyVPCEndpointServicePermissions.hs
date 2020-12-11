{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ModifyVPCEndpointServicePermissions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the permissions for your <https://docs.aws.amazon.com/vpc/latest/userguide/endpoint-service.html VPC endpoint service> . You can add or remove permissions for service consumers (IAM users, IAM roles, and AWS accounts) to connect to your endpoint service.
--
-- If you grant permissions to all principals, the service is public. Any users who know the name of a public service can send a request to attach an endpoint. If the service does not require manual approval, attachments are automatically approved.
module Network.AWS.EC2.ModifyVPCEndpointServicePermissions
  ( -- * Creating a request
    ModifyVPCEndpointServicePermissions (..),
    mkModifyVPCEndpointServicePermissions,

    -- ** Request lenses
    mvespRemoveAllowedPrincipals,
    mvespAddAllowedPrincipals,
    mvespDryRun,
    mvespServiceId,

    -- * Destructuring the response
    ModifyVPCEndpointServicePermissionsResponse (..),
    mkModifyVPCEndpointServicePermissionsResponse,

    -- ** Response lenses
    mvesprsReturnValue,
    mvesprsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkModifyVPCEndpointServicePermissions' smart constructor.
data ModifyVPCEndpointServicePermissions = ModifyVPCEndpointServicePermissions'
  { removeAllowedPrincipals ::
      Lude.Maybe
        [Lude.Text],
    addAllowedPrincipals ::
      Lude.Maybe
        [Lude.Text],
    dryRun ::
      Lude.Maybe
        Lude.Bool,
    serviceId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyVPCEndpointServicePermissions' with the minimum fields required to make a request.
--
-- * 'addAllowedPrincipals' - The Amazon Resource Names (ARN) of one or more principals. Permissions are granted to the principals in this list. To grant permissions to all principals, specify an asterisk (*).
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'removeAllowedPrincipals' - The Amazon Resource Names (ARN) of one or more principals. Permissions are revoked for principals in this list.
-- * 'serviceId' - The ID of the service.
mkModifyVPCEndpointServicePermissions ::
  -- | 'serviceId'
  Lude.Text ->
  ModifyVPCEndpointServicePermissions
mkModifyVPCEndpointServicePermissions pServiceId_ =
  ModifyVPCEndpointServicePermissions'
    { removeAllowedPrincipals =
        Lude.Nothing,
      addAllowedPrincipals = Lude.Nothing,
      dryRun = Lude.Nothing,
      serviceId = pServiceId_
    }

-- | The Amazon Resource Names (ARN) of one or more principals. Permissions are revoked for principals in this list.
--
-- /Note:/ Consider using 'removeAllowedPrincipals' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvespRemoveAllowedPrincipals :: Lens.Lens' ModifyVPCEndpointServicePermissions (Lude.Maybe [Lude.Text])
mvespRemoveAllowedPrincipals = Lens.lens (removeAllowedPrincipals :: ModifyVPCEndpointServicePermissions -> Lude.Maybe [Lude.Text]) (\s a -> s {removeAllowedPrincipals = a} :: ModifyVPCEndpointServicePermissions)
{-# DEPRECATED mvespRemoveAllowedPrincipals "Use generic-lens or generic-optics with 'removeAllowedPrincipals' instead." #-}

-- | The Amazon Resource Names (ARN) of one or more principals. Permissions are granted to the principals in this list. To grant permissions to all principals, specify an asterisk (*).
--
-- /Note:/ Consider using 'addAllowedPrincipals' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvespAddAllowedPrincipals :: Lens.Lens' ModifyVPCEndpointServicePermissions (Lude.Maybe [Lude.Text])
mvespAddAllowedPrincipals = Lens.lens (addAllowedPrincipals :: ModifyVPCEndpointServicePermissions -> Lude.Maybe [Lude.Text]) (\s a -> s {addAllowedPrincipals = a} :: ModifyVPCEndpointServicePermissions)
{-# DEPRECATED mvespAddAllowedPrincipals "Use generic-lens or generic-optics with 'addAllowedPrincipals' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvespDryRun :: Lens.Lens' ModifyVPCEndpointServicePermissions (Lude.Maybe Lude.Bool)
mvespDryRun = Lens.lens (dryRun :: ModifyVPCEndpointServicePermissions -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: ModifyVPCEndpointServicePermissions)
{-# DEPRECATED mvespDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The ID of the service.
--
-- /Note:/ Consider using 'serviceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvespServiceId :: Lens.Lens' ModifyVPCEndpointServicePermissions Lude.Text
mvespServiceId = Lens.lens (serviceId :: ModifyVPCEndpointServicePermissions -> Lude.Text) (\s a -> s {serviceId = a} :: ModifyVPCEndpointServicePermissions)
{-# DEPRECATED mvespServiceId "Use generic-lens or generic-optics with 'serviceId' instead." #-}

instance Lude.AWSRequest ModifyVPCEndpointServicePermissions where
  type
    Rs ModifyVPCEndpointServicePermissions =
      ModifyVPCEndpointServicePermissionsResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          ModifyVPCEndpointServicePermissionsResponse'
            Lude.<$> (x Lude..@? "return") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ModifyVPCEndpointServicePermissions where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ModifyVPCEndpointServicePermissions where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifyVPCEndpointServicePermissions where
  toQuery ModifyVPCEndpointServicePermissions' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("ModifyVpcEndpointServicePermissions" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery
          ( Lude.toQueryList "RemoveAllowedPrincipals"
              Lude.<$> removeAllowedPrincipals
          ),
        Lude.toQuery
          ( Lude.toQueryList "AddAllowedPrincipals"
              Lude.<$> addAllowedPrincipals
          ),
        "DryRun" Lude.=: dryRun,
        "ServiceId" Lude.=: serviceId
      ]

-- | /See:/ 'mkModifyVPCEndpointServicePermissionsResponse' smart constructor.
data ModifyVPCEndpointServicePermissionsResponse = ModifyVPCEndpointServicePermissionsResponse'
  { returnValue ::
      Lude.Maybe
        Lude.Bool,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyVPCEndpointServicePermissionsResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'returnValue' - Returns @true@ if the request succeeds; otherwise, it returns an error.
mkModifyVPCEndpointServicePermissionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ModifyVPCEndpointServicePermissionsResponse
mkModifyVPCEndpointServicePermissionsResponse pResponseStatus_ =
  ModifyVPCEndpointServicePermissionsResponse'
    { returnValue =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
--
-- /Note:/ Consider using 'returnValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvesprsReturnValue :: Lens.Lens' ModifyVPCEndpointServicePermissionsResponse (Lude.Maybe Lude.Bool)
mvesprsReturnValue = Lens.lens (returnValue :: ModifyVPCEndpointServicePermissionsResponse -> Lude.Maybe Lude.Bool) (\s a -> s {returnValue = a} :: ModifyVPCEndpointServicePermissionsResponse)
{-# DEPRECATED mvesprsReturnValue "Use generic-lens or generic-optics with 'returnValue' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvesprsResponseStatus :: Lens.Lens' ModifyVPCEndpointServicePermissionsResponse Lude.Int
mvesprsResponseStatus = Lens.lens (responseStatus :: ModifyVPCEndpointServicePermissionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ModifyVPCEndpointServicePermissionsResponse)
{-# DEPRECATED mvesprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

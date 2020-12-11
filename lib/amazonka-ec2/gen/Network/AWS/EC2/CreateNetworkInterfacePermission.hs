{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateNetworkInterfacePermission
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Grants an AWS-authorized account permission to attach the specified network interface to an instance in their account.
--
-- You can grant permission to a single AWS account only, and only one account at a time.
module Network.AWS.EC2.CreateNetworkInterfacePermission
  ( -- * Creating a request
    CreateNetworkInterfacePermission (..),
    mkCreateNetworkInterfacePermission,

    -- ** Request lenses
    cnipAWSAccountId,
    cnipAWSService,
    cnipDryRun,
    cnipNetworkInterfaceId,
    cnipPermission,

    -- * Destructuring the response
    CreateNetworkInterfacePermissionResponse (..),
    mkCreateNetworkInterfacePermissionResponse,

    -- ** Response lenses
    cniprsInterfacePermission,
    cniprsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for CreateNetworkInterfacePermission.
--
-- /See:/ 'mkCreateNetworkInterfacePermission' smart constructor.
data CreateNetworkInterfacePermission = CreateNetworkInterfacePermission'
  { awsAccountId ::
      Lude.Maybe Lude.Text,
    awsService ::
      Lude.Maybe Lude.Text,
    dryRun ::
      Lude.Maybe Lude.Bool,
    networkInterfaceId ::
      Lude.Text,
    permission ::
      InterfacePermissionType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateNetworkInterfacePermission' with the minimum fields required to make a request.
--
-- * 'awsAccountId' - The AWS account ID.
-- * 'awsService' - The AWS service. Currently not supported.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'networkInterfaceId' - The ID of the network interface.
-- * 'permission' - The type of permission to grant.
mkCreateNetworkInterfacePermission ::
  -- | 'networkInterfaceId'
  Lude.Text ->
  -- | 'permission'
  InterfacePermissionType ->
  CreateNetworkInterfacePermission
mkCreateNetworkInterfacePermission
  pNetworkInterfaceId_
  pPermission_ =
    CreateNetworkInterfacePermission'
      { awsAccountId = Lude.Nothing,
        awsService = Lude.Nothing,
        dryRun = Lude.Nothing,
        networkInterfaceId = pNetworkInterfaceId_,
        permission = pPermission_
      }

-- | The AWS account ID.
--
-- /Note:/ Consider using 'awsAccountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnipAWSAccountId :: Lens.Lens' CreateNetworkInterfacePermission (Lude.Maybe Lude.Text)
cnipAWSAccountId = Lens.lens (awsAccountId :: CreateNetworkInterfacePermission -> Lude.Maybe Lude.Text) (\s a -> s {awsAccountId = a} :: CreateNetworkInterfacePermission)
{-# DEPRECATED cnipAWSAccountId "Use generic-lens or generic-optics with 'awsAccountId' instead." #-}

-- | The AWS service. Currently not supported.
--
-- /Note:/ Consider using 'awsService' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnipAWSService :: Lens.Lens' CreateNetworkInterfacePermission (Lude.Maybe Lude.Text)
cnipAWSService = Lens.lens (awsService :: CreateNetworkInterfacePermission -> Lude.Maybe Lude.Text) (\s a -> s {awsService = a} :: CreateNetworkInterfacePermission)
{-# DEPRECATED cnipAWSService "Use generic-lens or generic-optics with 'awsService' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnipDryRun :: Lens.Lens' CreateNetworkInterfacePermission (Lude.Maybe Lude.Bool)
cnipDryRun = Lens.lens (dryRun :: CreateNetworkInterfacePermission -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: CreateNetworkInterfacePermission)
{-# DEPRECATED cnipDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The ID of the network interface.
--
-- /Note:/ Consider using 'networkInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnipNetworkInterfaceId :: Lens.Lens' CreateNetworkInterfacePermission Lude.Text
cnipNetworkInterfaceId = Lens.lens (networkInterfaceId :: CreateNetworkInterfacePermission -> Lude.Text) (\s a -> s {networkInterfaceId = a} :: CreateNetworkInterfacePermission)
{-# DEPRECATED cnipNetworkInterfaceId "Use generic-lens or generic-optics with 'networkInterfaceId' instead." #-}

-- | The type of permission to grant.
--
-- /Note:/ Consider using 'permission' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnipPermission :: Lens.Lens' CreateNetworkInterfacePermission InterfacePermissionType
cnipPermission = Lens.lens (permission :: CreateNetworkInterfacePermission -> InterfacePermissionType) (\s a -> s {permission = a} :: CreateNetworkInterfacePermission)
{-# DEPRECATED cnipPermission "Use generic-lens or generic-optics with 'permission' instead." #-}

instance Lude.AWSRequest CreateNetworkInterfacePermission where
  type
    Rs CreateNetworkInterfacePermission =
      CreateNetworkInterfacePermissionResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          CreateNetworkInterfacePermissionResponse'
            Lude.<$> (x Lude..@? "interfacePermission")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateNetworkInterfacePermission where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateNetworkInterfacePermission where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateNetworkInterfacePermission where
  toQuery CreateNetworkInterfacePermission' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("CreateNetworkInterfacePermission" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "AwsAccountId" Lude.=: awsAccountId,
        "AwsService" Lude.=: awsService,
        "DryRun" Lude.=: dryRun,
        "NetworkInterfaceId" Lude.=: networkInterfaceId,
        "Permission" Lude.=: permission
      ]

-- | Contains the output of CreateNetworkInterfacePermission.
--
-- /See:/ 'mkCreateNetworkInterfacePermissionResponse' smart constructor.
data CreateNetworkInterfacePermissionResponse = CreateNetworkInterfacePermissionResponse'
  { interfacePermission ::
      Lude.Maybe
        NetworkInterfacePermission,
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

-- | Creates a value of 'CreateNetworkInterfacePermissionResponse' with the minimum fields required to make a request.
--
-- * 'interfacePermission' - Information about the permission for the network interface.
-- * 'responseStatus' - The response status code.
mkCreateNetworkInterfacePermissionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateNetworkInterfacePermissionResponse
mkCreateNetworkInterfacePermissionResponse pResponseStatus_ =
  CreateNetworkInterfacePermissionResponse'
    { interfacePermission =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the permission for the network interface.
--
-- /Note:/ Consider using 'interfacePermission' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cniprsInterfacePermission :: Lens.Lens' CreateNetworkInterfacePermissionResponse (Lude.Maybe NetworkInterfacePermission)
cniprsInterfacePermission = Lens.lens (interfacePermission :: CreateNetworkInterfacePermissionResponse -> Lude.Maybe NetworkInterfacePermission) (\s a -> s {interfacePermission = a} :: CreateNetworkInterfacePermissionResponse)
{-# DEPRECATED cniprsInterfacePermission "Use generic-lens or generic-optics with 'interfacePermission' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cniprsResponseStatus :: Lens.Lens' CreateNetworkInterfacePermissionResponse Lude.Int
cniprsResponseStatus = Lens.lens (responseStatus :: CreateNetworkInterfacePermissionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateNetworkInterfacePermissionResponse)
{-# DEPRECATED cniprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

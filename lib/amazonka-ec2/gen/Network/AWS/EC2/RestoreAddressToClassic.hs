{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.RestoreAddressToClassic
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Restores an Elastic IP address that was previously moved to the EC2-VPC platform back to the EC2-Classic platform. You cannot move an Elastic IP address that was originally allocated for use in EC2-VPC. The Elastic IP address must not be associated with an instance or network interface.
module Network.AWS.EC2.RestoreAddressToClassic
  ( -- * Creating a request
    RestoreAddressToClassic (..),
    mkRestoreAddressToClassic,

    -- ** Request lenses
    ratcPublicIP,
    ratcDryRun,

    -- * Destructuring the response
    RestoreAddressToClassicResponse (..),
    mkRestoreAddressToClassicResponse,

    -- ** Response lenses
    ratcrsStatus,
    ratcrsPublicIP,
    ratcrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkRestoreAddressToClassic' smart constructor.
data RestoreAddressToClassic = RestoreAddressToClassic'
  { -- | The Elastic IP address.
    publicIP :: Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RestoreAddressToClassic' with the minimum fields required to make a request.
--
-- * 'publicIP' - The Elastic IP address.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkRestoreAddressToClassic ::
  -- | 'publicIP'
  Lude.Text ->
  RestoreAddressToClassic
mkRestoreAddressToClassic pPublicIP_ =
  RestoreAddressToClassic'
    { publicIP = pPublicIP_,
      dryRun = Lude.Nothing
    }

-- | The Elastic IP address.
--
-- /Note:/ Consider using 'publicIP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ratcPublicIP :: Lens.Lens' RestoreAddressToClassic Lude.Text
ratcPublicIP = Lens.lens (publicIP :: RestoreAddressToClassic -> Lude.Text) (\s a -> s {publicIP = a} :: RestoreAddressToClassic)
{-# DEPRECATED ratcPublicIP "Use generic-lens or generic-optics with 'publicIP' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ratcDryRun :: Lens.Lens' RestoreAddressToClassic (Lude.Maybe Lude.Bool)
ratcDryRun = Lens.lens (dryRun :: RestoreAddressToClassic -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: RestoreAddressToClassic)
{-# DEPRECATED ratcDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest RestoreAddressToClassic where
  type Rs RestoreAddressToClassic = RestoreAddressToClassicResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          RestoreAddressToClassicResponse'
            Lude.<$> (x Lude..@? "status")
            Lude.<*> (x Lude..@? "publicIp")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RestoreAddressToClassic where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath RestoreAddressToClassic where
  toPath = Lude.const "/"

instance Lude.ToQuery RestoreAddressToClassic where
  toQuery RestoreAddressToClassic' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("RestoreAddressToClassic" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "PublicIp" Lude.=: publicIP,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkRestoreAddressToClassicResponse' smart constructor.
data RestoreAddressToClassicResponse = RestoreAddressToClassicResponse'
  { -- | The move status for the IP address.
    status :: Lude.Maybe AddressStatus,
    -- | The Elastic IP address.
    publicIP :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RestoreAddressToClassicResponse' with the minimum fields required to make a request.
--
-- * 'status' - The move status for the IP address.
-- * 'publicIP' - The Elastic IP address.
-- * 'responseStatus' - The response status code.
mkRestoreAddressToClassicResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RestoreAddressToClassicResponse
mkRestoreAddressToClassicResponse pResponseStatus_ =
  RestoreAddressToClassicResponse'
    { status = Lude.Nothing,
      publicIP = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The move status for the IP address.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ratcrsStatus :: Lens.Lens' RestoreAddressToClassicResponse (Lude.Maybe AddressStatus)
ratcrsStatus = Lens.lens (status :: RestoreAddressToClassicResponse -> Lude.Maybe AddressStatus) (\s a -> s {status = a} :: RestoreAddressToClassicResponse)
{-# DEPRECATED ratcrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The Elastic IP address.
--
-- /Note:/ Consider using 'publicIP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ratcrsPublicIP :: Lens.Lens' RestoreAddressToClassicResponse (Lude.Maybe Lude.Text)
ratcrsPublicIP = Lens.lens (publicIP :: RestoreAddressToClassicResponse -> Lude.Maybe Lude.Text) (\s a -> s {publicIP = a} :: RestoreAddressToClassicResponse)
{-# DEPRECATED ratcrsPublicIP "Use generic-lens or generic-optics with 'publicIP' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ratcrsResponseStatus :: Lens.Lens' RestoreAddressToClassicResponse Lude.Int
ratcrsResponseStatus = Lens.lens (responseStatus :: RestoreAddressToClassicResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RestoreAddressToClassicResponse)
{-# DEPRECATED ratcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

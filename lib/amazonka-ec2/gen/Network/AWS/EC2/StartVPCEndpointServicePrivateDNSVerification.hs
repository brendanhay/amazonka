{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.StartVPCEndpointServicePrivateDNSVerification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Initiates the verification process to prove that the service provider owns the private DNS name domain for the endpoint service.
--
-- The service provider must successfully perform the verification before the consumer can use the name to access the service.
-- Before the service provider runs this command, they must add a record to the DNS server. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/endpoint-services-dns-validation.html#add-dns-txt-record Adding a TXT Record to Your Domain's DNS Server > in the /Amazon VPC User Guide/ .
module Network.AWS.EC2.StartVPCEndpointServicePrivateDNSVerification
  ( -- * Creating a request
    StartVPCEndpointServicePrivateDNSVerification (..),
    mkStartVPCEndpointServicePrivateDNSVerification,

    -- ** Request lenses
    svespdvServiceId,
    svespdvDryRun,

    -- * Destructuring the response
    StartVPCEndpointServicePrivateDNSVerificationResponse (..),
    mkStartVPCEndpointServicePrivateDNSVerificationResponse,

    -- ** Response lenses
    svespdvrsReturnValue,
    svespdvrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStartVPCEndpointServicePrivateDNSVerification' smart constructor.
data StartVPCEndpointServicePrivateDNSVerification = StartVPCEndpointServicePrivateDNSVerification'
  { -- | The ID of the endpoint service.
    serviceId :: Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartVPCEndpointServicePrivateDNSVerification' with the minimum fields required to make a request.
--
-- * 'serviceId' - The ID of the endpoint service.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkStartVPCEndpointServicePrivateDNSVerification ::
  -- | 'serviceId'
  Lude.Text ->
  StartVPCEndpointServicePrivateDNSVerification
mkStartVPCEndpointServicePrivateDNSVerification pServiceId_ =
  StartVPCEndpointServicePrivateDNSVerification'
    { serviceId =
        pServiceId_,
      dryRun = Lude.Nothing
    }

-- | The ID of the endpoint service.
--
-- /Note:/ Consider using 'serviceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
svespdvServiceId :: Lens.Lens' StartVPCEndpointServicePrivateDNSVerification Lude.Text
svespdvServiceId = Lens.lens (serviceId :: StartVPCEndpointServicePrivateDNSVerification -> Lude.Text) (\s a -> s {serviceId = a} :: StartVPCEndpointServicePrivateDNSVerification)
{-# DEPRECATED svespdvServiceId "Use generic-lens or generic-optics with 'serviceId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
svespdvDryRun :: Lens.Lens' StartVPCEndpointServicePrivateDNSVerification (Lude.Maybe Lude.Bool)
svespdvDryRun = Lens.lens (dryRun :: StartVPCEndpointServicePrivateDNSVerification -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: StartVPCEndpointServicePrivateDNSVerification)
{-# DEPRECATED svespdvDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance
  Lude.AWSRequest
    StartVPCEndpointServicePrivateDNSVerification
  where
  type
    Rs StartVPCEndpointServicePrivateDNSVerification =
      StartVPCEndpointServicePrivateDNSVerificationResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          StartVPCEndpointServicePrivateDNSVerificationResponse'
            Lude.<$> (x Lude..@? "return") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance
  Lude.ToHeaders
    StartVPCEndpointServicePrivateDNSVerification
  where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath StartVPCEndpointServicePrivateDNSVerification where
  toPath = Lude.const "/"

instance Lude.ToQuery StartVPCEndpointServicePrivateDNSVerification where
  toQuery StartVPCEndpointServicePrivateDNSVerification' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ( "StartVpcEndpointServicePrivateDnsVerification" ::
                      Lude.ByteString
                  ),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "ServiceId" Lude.=: serviceId,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkStartVPCEndpointServicePrivateDNSVerificationResponse' smart constructor.
data StartVPCEndpointServicePrivateDNSVerificationResponse = StartVPCEndpointServicePrivateDNSVerificationResponse'
  { -- | Returns @true@ if the request succeeds; otherwise, it returns an error.
    returnValue :: Lude.Maybe Lude.Bool,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartVPCEndpointServicePrivateDNSVerificationResponse' with the minimum fields required to make a request.
--
-- * 'returnValue' - Returns @true@ if the request succeeds; otherwise, it returns an error.
-- * 'responseStatus' - The response status code.
mkStartVPCEndpointServicePrivateDNSVerificationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartVPCEndpointServicePrivateDNSVerificationResponse
mkStartVPCEndpointServicePrivateDNSVerificationResponse
  pResponseStatus_ =
    StartVPCEndpointServicePrivateDNSVerificationResponse'
      { returnValue =
          Lude.Nothing,
        responseStatus = pResponseStatus_
      }

-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
--
-- /Note:/ Consider using 'returnValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
svespdvrsReturnValue :: Lens.Lens' StartVPCEndpointServicePrivateDNSVerificationResponse (Lude.Maybe Lude.Bool)
svespdvrsReturnValue = Lens.lens (returnValue :: StartVPCEndpointServicePrivateDNSVerificationResponse -> Lude.Maybe Lude.Bool) (\s a -> s {returnValue = a} :: StartVPCEndpointServicePrivateDNSVerificationResponse)
{-# DEPRECATED svespdvrsReturnValue "Use generic-lens or generic-optics with 'returnValue' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
svespdvrsResponseStatus :: Lens.Lens' StartVPCEndpointServicePrivateDNSVerificationResponse Lude.Int
svespdvrsResponseStatus = Lens.lens (responseStatus :: StartVPCEndpointServicePrivateDNSVerificationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartVPCEndpointServicePrivateDNSVerificationResponse)
{-# DEPRECATED svespdvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

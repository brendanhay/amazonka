{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.GetAssociatedEnclaveCertificateIAMRoles
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the IAM roles that are associated with the specified AWS Certificate Manager (ACM) certificate. It also returns the name of the Amazon S3 bucket and the Amazon S3 object key where the certificate, certificate chain, and encrypted private key bundle are stored, and the ARN of the AWS Key Management Service (KMS) customer master key (CMK) that's used to encrypt the private key.
module Network.AWS.EC2.GetAssociatedEnclaveCertificateIAMRoles
  ( -- * Creating a request
    GetAssociatedEnclaveCertificateIAMRoles (..),
    mkGetAssociatedEnclaveCertificateIAMRoles,

    -- ** Request lenses
    gaecirCertificateARN,
    gaecirDryRun,

    -- * Destructuring the response
    GetAssociatedEnclaveCertificateIAMRolesResponse (..),
    mkGetAssociatedEnclaveCertificateIAMRolesResponse,

    -- ** Response lenses
    gaecirrsAssociatedRoles,
    gaecirrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetAssociatedEnclaveCertificateIAMRoles' smart constructor.
data GetAssociatedEnclaveCertificateIAMRoles = GetAssociatedEnclaveCertificateIAMRoles'
  { -- | The ARN of the ACM certificate for which to view the associated IAM roles, encryption keys, and Amazon S3 object information.
    certificateARN :: Lude.Maybe Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetAssociatedEnclaveCertificateIAMRoles' with the minimum fields required to make a request.
--
-- * 'certificateARN' - The ARN of the ACM certificate for which to view the associated IAM roles, encryption keys, and Amazon S3 object information.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkGetAssociatedEnclaveCertificateIAMRoles ::
  GetAssociatedEnclaveCertificateIAMRoles
mkGetAssociatedEnclaveCertificateIAMRoles =
  GetAssociatedEnclaveCertificateIAMRoles'
    { certificateARN =
        Lude.Nothing,
      dryRun = Lude.Nothing
    }

-- | The ARN of the ACM certificate for which to view the associated IAM roles, encryption keys, and Amazon S3 object information.
--
-- /Note:/ Consider using 'certificateARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaecirCertificateARN :: Lens.Lens' GetAssociatedEnclaveCertificateIAMRoles (Lude.Maybe Lude.Text)
gaecirCertificateARN = Lens.lens (certificateARN :: GetAssociatedEnclaveCertificateIAMRoles -> Lude.Maybe Lude.Text) (\s a -> s {certificateARN = a} :: GetAssociatedEnclaveCertificateIAMRoles)
{-# DEPRECATED gaecirCertificateARN "Use generic-lens or generic-optics with 'certificateARN' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaecirDryRun :: Lens.Lens' GetAssociatedEnclaveCertificateIAMRoles (Lude.Maybe Lude.Bool)
gaecirDryRun = Lens.lens (dryRun :: GetAssociatedEnclaveCertificateIAMRoles -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: GetAssociatedEnclaveCertificateIAMRoles)
{-# DEPRECATED gaecirDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest GetAssociatedEnclaveCertificateIAMRoles where
  type
    Rs GetAssociatedEnclaveCertificateIAMRoles =
      GetAssociatedEnclaveCertificateIAMRolesResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          GetAssociatedEnclaveCertificateIAMRolesResponse'
            Lude.<$> ( x Lude..@? "associatedRoleSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetAssociatedEnclaveCertificateIAMRoles where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetAssociatedEnclaveCertificateIAMRoles where
  toPath = Lude.const "/"

instance Lude.ToQuery GetAssociatedEnclaveCertificateIAMRoles where
  toQuery GetAssociatedEnclaveCertificateIAMRoles' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("GetAssociatedEnclaveCertificateIamRoles" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "CertificateArn" Lude.=: certificateARN,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkGetAssociatedEnclaveCertificateIAMRolesResponse' smart constructor.
data GetAssociatedEnclaveCertificateIAMRolesResponse = GetAssociatedEnclaveCertificateIAMRolesResponse'
  { -- | Information about the associated IAM roles.
    associatedRoles :: Lude.Maybe [AssociatedRole],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetAssociatedEnclaveCertificateIAMRolesResponse' with the minimum fields required to make a request.
--
-- * 'associatedRoles' - Information about the associated IAM roles.
-- * 'responseStatus' - The response status code.
mkGetAssociatedEnclaveCertificateIAMRolesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetAssociatedEnclaveCertificateIAMRolesResponse
mkGetAssociatedEnclaveCertificateIAMRolesResponse pResponseStatus_ =
  GetAssociatedEnclaveCertificateIAMRolesResponse'
    { associatedRoles =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the associated IAM roles.
--
-- /Note:/ Consider using 'associatedRoles' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaecirrsAssociatedRoles :: Lens.Lens' GetAssociatedEnclaveCertificateIAMRolesResponse (Lude.Maybe [AssociatedRole])
gaecirrsAssociatedRoles = Lens.lens (associatedRoles :: GetAssociatedEnclaveCertificateIAMRolesResponse -> Lude.Maybe [AssociatedRole]) (\s a -> s {associatedRoles = a} :: GetAssociatedEnclaveCertificateIAMRolesResponse)
{-# DEPRECATED gaecirrsAssociatedRoles "Use generic-lens or generic-optics with 'associatedRoles' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaecirrsResponseStatus :: Lens.Lens' GetAssociatedEnclaveCertificateIAMRolesResponse Lude.Int
gaecirrsResponseStatus = Lens.lens (responseStatus :: GetAssociatedEnclaveCertificateIAMRolesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetAssociatedEnclaveCertificateIAMRolesResponse)
{-# DEPRECATED gaecirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

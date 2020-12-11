{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DisassociateEnclaveCertificateIAMRole
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates an IAM role from an AWS Certificate Manager (ACM) certificate. Disassociating an IAM role from an ACM certificate removes the Amazon S3 object that contains the certificate, certificate chain, and encrypted private key from the Amazon S3 bucket. It also revokes the IAM role's permission to use the AWS Key Management Service (KMS) customer master key (CMK) used to encrypt the private key. This effectively revokes the role's permission to use the certificate.
module Network.AWS.EC2.DisassociateEnclaveCertificateIAMRole
  ( -- * Creating a request
    DisassociateEnclaveCertificateIAMRole (..),
    mkDisassociateEnclaveCertificateIAMRole,

    -- ** Request lenses
    decirCertificateARN,
    decirDryRun,
    decirRoleARN,

    -- * Destructuring the response
    DisassociateEnclaveCertificateIAMRoleResponse (..),
    mkDisassociateEnclaveCertificateIAMRoleResponse,

    -- ** Response lenses
    decirrsReturn,
    decirrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDisassociateEnclaveCertificateIAMRole' smart constructor.
data DisassociateEnclaveCertificateIAMRole = DisassociateEnclaveCertificateIAMRole'
  { certificateARN ::
      Lude.Maybe
        Lude.Text,
    dryRun ::
      Lude.Maybe
        Lude.Bool,
    roleARN ::
      Lude.Maybe
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

-- | Creates a value of 'DisassociateEnclaveCertificateIAMRole' with the minimum fields required to make a request.
--
-- * 'certificateARN' - The ARN of the ACM certificate from which to disassociate the IAM role.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'roleARN' - The ARN of the IAM role to disassociate.
mkDisassociateEnclaveCertificateIAMRole ::
  DisassociateEnclaveCertificateIAMRole
mkDisassociateEnclaveCertificateIAMRole =
  DisassociateEnclaveCertificateIAMRole'
    { certificateARN =
        Lude.Nothing,
      dryRun = Lude.Nothing,
      roleARN = Lude.Nothing
    }

-- | The ARN of the ACM certificate from which to disassociate the IAM role.
--
-- /Note:/ Consider using 'certificateARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decirCertificateARN :: Lens.Lens' DisassociateEnclaveCertificateIAMRole (Lude.Maybe Lude.Text)
decirCertificateARN = Lens.lens (certificateARN :: DisassociateEnclaveCertificateIAMRole -> Lude.Maybe Lude.Text) (\s a -> s {certificateARN = a} :: DisassociateEnclaveCertificateIAMRole)
{-# DEPRECATED decirCertificateARN "Use generic-lens or generic-optics with 'certificateARN' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decirDryRun :: Lens.Lens' DisassociateEnclaveCertificateIAMRole (Lude.Maybe Lude.Bool)
decirDryRun = Lens.lens (dryRun :: DisassociateEnclaveCertificateIAMRole -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DisassociateEnclaveCertificateIAMRole)
{-# DEPRECATED decirDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The ARN of the IAM role to disassociate.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decirRoleARN :: Lens.Lens' DisassociateEnclaveCertificateIAMRole (Lude.Maybe Lude.Text)
decirRoleARN = Lens.lens (roleARN :: DisassociateEnclaveCertificateIAMRole -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: DisassociateEnclaveCertificateIAMRole)
{-# DEPRECATED decirRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.AWSRequest DisassociateEnclaveCertificateIAMRole where
  type
    Rs DisassociateEnclaveCertificateIAMRole =
      DisassociateEnclaveCertificateIAMRoleResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DisassociateEnclaveCertificateIAMRoleResponse'
            Lude.<$> (x Lude..@? "return") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DisassociateEnclaveCertificateIAMRole where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DisassociateEnclaveCertificateIAMRole where
  toPath = Lude.const "/"

instance Lude.ToQuery DisassociateEnclaveCertificateIAMRole where
  toQuery DisassociateEnclaveCertificateIAMRole' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DisassociateEnclaveCertificateIamRole" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "CertificateArn" Lude.=: certificateARN,
        "DryRun" Lude.=: dryRun,
        "RoleArn" Lude.=: roleARN
      ]

-- | /See:/ 'mkDisassociateEnclaveCertificateIAMRoleResponse' smart constructor.
data DisassociateEnclaveCertificateIAMRoleResponse = DisassociateEnclaveCertificateIAMRoleResponse'
  { return ::
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
  deriving anyclass
    ( Lude.Hashable,
      Lude.NFData
    )

-- | Creates a value of 'DisassociateEnclaveCertificateIAMRoleResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'return' - Returns @true@ if the request succeeds; otherwise, it returns an error.
mkDisassociateEnclaveCertificateIAMRoleResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DisassociateEnclaveCertificateIAMRoleResponse
mkDisassociateEnclaveCertificateIAMRoleResponse pResponseStatus_ =
  DisassociateEnclaveCertificateIAMRoleResponse'
    { return =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
--
-- /Note:/ Consider using 'return' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decirrsReturn :: Lens.Lens' DisassociateEnclaveCertificateIAMRoleResponse (Lude.Maybe Lude.Bool)
decirrsReturn = Lens.lens (return :: DisassociateEnclaveCertificateIAMRoleResponse -> Lude.Maybe Lude.Bool) (\s a -> s {return = a} :: DisassociateEnclaveCertificateIAMRoleResponse)
{-# DEPRECATED decirrsReturn "Use generic-lens or generic-optics with 'return' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decirrsResponseStatus :: Lens.Lens' DisassociateEnclaveCertificateIAMRoleResponse Lude.Int
decirrsResponseStatus = Lens.lens (responseStatus :: DisassociateEnclaveCertificateIAMRoleResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DisassociateEnclaveCertificateIAMRoleResponse)
{-# DEPRECATED decirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

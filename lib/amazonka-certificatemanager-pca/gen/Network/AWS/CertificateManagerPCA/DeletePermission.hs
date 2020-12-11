{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManagerPCA.DeletePermission
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Revokes permissions on a private CA granted to the AWS Certificate Manager (ACM) service principal (acm.amazonaws.com).
--
-- These permissions allow ACM to issue and renew ACM certificates that reside in the same AWS account as the CA. If you revoke these permissions, ACM will no longer renew the affected certificates automatically.
-- Permissions can be granted with the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreatePermission.html CreatePermission> action and listed with the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_ListPermissions.html ListPermissions> action.
-- __About Permissions__
--
--     * If the private CA and the certificates it issues reside in the same account, you can use @CreatePermission@ to grant permissions for ACM to carry out automatic certificate renewals.
--
--
--     * For automatic certificate renewal to succeed, the ACM service principal needs permissions to create, retrieve, and list certificates.
--
--
--     * If the private CA and the ACM certificates reside in different accounts, then permissions cannot be used to enable automatic renewals. Instead, the ACM certificate owner must set up a resource-based policy to enable cross-account issuance and renewals. For more information, see <acm-pca/latest/userguide/pca-rbp.html Using a Resource Based Policy with ACM Private CA> .
module Network.AWS.CertificateManagerPCA.DeletePermission
  ( -- * Creating a request
    DeletePermission (..),
    mkDeletePermission,

    -- ** Request lenses
    dpSourceAccount,
    dpCertificateAuthorityARN,
    dpPrincipal,

    -- * Destructuring the response
    DeletePermissionResponse (..),
    mkDeletePermissionResponse,
  )
where

import Network.AWS.CertificateManagerPCA.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeletePermission' smart constructor.
data DeletePermission = DeletePermission'
  { sourceAccount ::
      Lude.Maybe Lude.Text,
    certificateAuthorityARN :: Lude.Text,
    principal :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeletePermission' with the minimum fields required to make a request.
--
-- * 'certificateAuthorityARN' - The Amazon Resource Number (ARN) of the private CA that issued the permissions. You can find the CA's ARN by calling the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_ListCertificateAuthorities.html ListCertificateAuthorities> action. This must have the following form:
--
-- @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @ .
-- * 'principal' - The AWS service or identity that will have its CA permissions revoked. At this time, the only valid service principal is @acm.amazonaws.com@
-- * 'sourceAccount' - The AWS account that calls this action.
mkDeletePermission ::
  -- | 'certificateAuthorityARN'
  Lude.Text ->
  -- | 'principal'
  Lude.Text ->
  DeletePermission
mkDeletePermission pCertificateAuthorityARN_ pPrincipal_ =
  DeletePermission'
    { sourceAccount = Lude.Nothing,
      certificateAuthorityARN = pCertificateAuthorityARN_,
      principal = pPrincipal_
    }

-- | The AWS account that calls this action.
--
-- /Note:/ Consider using 'sourceAccount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpSourceAccount :: Lens.Lens' DeletePermission (Lude.Maybe Lude.Text)
dpSourceAccount = Lens.lens (sourceAccount :: DeletePermission -> Lude.Maybe Lude.Text) (\s a -> s {sourceAccount = a} :: DeletePermission)
{-# DEPRECATED dpSourceAccount "Use generic-lens or generic-optics with 'sourceAccount' instead." #-}

-- | The Amazon Resource Number (ARN) of the private CA that issued the permissions. You can find the CA's ARN by calling the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_ListCertificateAuthorities.html ListCertificateAuthorities> action. This must have the following form:
--
-- @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @ .
--
-- /Note:/ Consider using 'certificateAuthorityARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpCertificateAuthorityARN :: Lens.Lens' DeletePermission Lude.Text
dpCertificateAuthorityARN = Lens.lens (certificateAuthorityARN :: DeletePermission -> Lude.Text) (\s a -> s {certificateAuthorityARN = a} :: DeletePermission)
{-# DEPRECATED dpCertificateAuthorityARN "Use generic-lens or generic-optics with 'certificateAuthorityARN' instead." #-}

-- | The AWS service or identity that will have its CA permissions revoked. At this time, the only valid service principal is @acm.amazonaws.com@
--
-- /Note:/ Consider using 'principal' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpPrincipal :: Lens.Lens' DeletePermission Lude.Text
dpPrincipal = Lens.lens (principal :: DeletePermission -> Lude.Text) (\s a -> s {principal = a} :: DeletePermission)
{-# DEPRECATED dpPrincipal "Use generic-lens or generic-optics with 'principal' instead." #-}

instance Lude.AWSRequest DeletePermission where
  type Rs DeletePermission = DeletePermissionResponse
  request = Req.postJSON certificateManagerPCAService
  response = Res.receiveNull DeletePermissionResponse'

instance Lude.ToHeaders DeletePermission where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("ACMPrivateCA.DeletePermission" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeletePermission where
  toJSON DeletePermission' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SourceAccount" Lude..=) Lude.<$> sourceAccount,
            Lude.Just
              ("CertificateAuthorityArn" Lude..= certificateAuthorityARN),
            Lude.Just ("Principal" Lude..= principal)
          ]
      )

instance Lude.ToPath DeletePermission where
  toPath = Lude.const "/"

instance Lude.ToQuery DeletePermission where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeletePermissionResponse' smart constructor.
data DeletePermissionResponse = DeletePermissionResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeletePermissionResponse' with the minimum fields required to make a request.
mkDeletePermissionResponse ::
  DeletePermissionResponse
mkDeletePermissionResponse = DeletePermissionResponse'

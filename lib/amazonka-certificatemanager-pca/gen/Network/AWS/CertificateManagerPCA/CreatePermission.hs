{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManagerPCA.CreatePermission
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Grants one or more permissions on a private CA to the AWS Certificate Manager (ACM) service principal (@acm.amazonaws.com@ ). These permissions allow ACM to issue and renew ACM certificates that reside in the same AWS account as the CA.
--
-- You can list current permissions with the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_ListPermissions.html ListPermissions> action and revoke them with the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_DeletePermission.html DeletePermission> action.
-- __About Permissions__
--
--     * If the private CA and the certificates it issues reside in the same account, you can use @CreatePermission@ to grant permissions for ACM to carry out automatic certificate renewals.
--
--
--     * For automatic certificate renewal to succeed, the ACM service principal needs permissions to create, retrieve, and list certificates.
--
--
--     * If the private CA and the ACM certificates reside in different accounts, then permissions cannot be used to enable automatic renewals. Instead, the ACM certificate owner must set up a resource-based policy to enable cross-account issuance and renewals. For more information, see <acm-pca/latest/userguide/pca-rbp.html Using a Resource Based Policy with ACM Private CA> .
module Network.AWS.CertificateManagerPCA.CreatePermission
  ( -- * Creating a request
    CreatePermission (..),
    mkCreatePermission,

    -- ** Request lenses
    cpSourceAccount,
    cpActions,
    cpPrincipal,
    cpCertificateAuthorityARN,

    -- * Destructuring the response
    CreatePermissionResponse (..),
    mkCreatePermissionResponse,
  )
where

import Network.AWS.CertificateManagerPCA.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreatePermission' smart constructor.
data CreatePermission = CreatePermission'
  { -- | The ID of the calling account.
    sourceAccount :: Lude.Maybe Lude.Text,
    -- | The actions that the specified AWS service principal can use. These include @IssueCertificate@ , @GetCertificate@ , and @ListPermissions@ .
    actions :: Lude.NonEmpty ActionType,
    -- | The AWS service or identity that receives the permission. At this time, the only valid principal is @acm.amazonaws.com@ .
    principal :: Lude.Text,
    -- | The Amazon Resource Name (ARN) of the CA that grants the permissions. You can find the ARN by calling the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_ListCertificateAuthorities.html ListCertificateAuthorities> action. This must have the following form:
    --
    -- @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @ .
    certificateAuthorityARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreatePermission' with the minimum fields required to make a request.
--
-- * 'sourceAccount' - The ID of the calling account.
-- * 'actions' - The actions that the specified AWS service principal can use. These include @IssueCertificate@ , @GetCertificate@ , and @ListPermissions@ .
-- * 'principal' - The AWS service or identity that receives the permission. At this time, the only valid principal is @acm.amazonaws.com@ .
-- * 'certificateAuthorityARN' - The Amazon Resource Name (ARN) of the CA that grants the permissions. You can find the ARN by calling the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_ListCertificateAuthorities.html ListCertificateAuthorities> action. This must have the following form:
--
-- @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @ .
mkCreatePermission ::
  -- | 'actions'
  Lude.NonEmpty ActionType ->
  -- | 'principal'
  Lude.Text ->
  -- | 'certificateAuthorityARN'
  Lude.Text ->
  CreatePermission
mkCreatePermission pActions_ pPrincipal_ pCertificateAuthorityARN_ =
  CreatePermission'
    { sourceAccount = Lude.Nothing,
      actions = pActions_,
      principal = pPrincipal_,
      certificateAuthorityARN = pCertificateAuthorityARN_
    }

-- | The ID of the calling account.
--
-- /Note:/ Consider using 'sourceAccount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpSourceAccount :: Lens.Lens' CreatePermission (Lude.Maybe Lude.Text)
cpSourceAccount = Lens.lens (sourceAccount :: CreatePermission -> Lude.Maybe Lude.Text) (\s a -> s {sourceAccount = a} :: CreatePermission)
{-# DEPRECATED cpSourceAccount "Use generic-lens or generic-optics with 'sourceAccount' instead." #-}

-- | The actions that the specified AWS service principal can use. These include @IssueCertificate@ , @GetCertificate@ , and @ListPermissions@ .
--
-- /Note:/ Consider using 'actions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpActions :: Lens.Lens' CreatePermission (Lude.NonEmpty ActionType)
cpActions = Lens.lens (actions :: CreatePermission -> Lude.NonEmpty ActionType) (\s a -> s {actions = a} :: CreatePermission)
{-# DEPRECATED cpActions "Use generic-lens or generic-optics with 'actions' instead." #-}

-- | The AWS service or identity that receives the permission. At this time, the only valid principal is @acm.amazonaws.com@ .
--
-- /Note:/ Consider using 'principal' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpPrincipal :: Lens.Lens' CreatePermission Lude.Text
cpPrincipal = Lens.lens (principal :: CreatePermission -> Lude.Text) (\s a -> s {principal = a} :: CreatePermission)
{-# DEPRECATED cpPrincipal "Use generic-lens or generic-optics with 'principal' instead." #-}

-- | The Amazon Resource Name (ARN) of the CA that grants the permissions. You can find the ARN by calling the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_ListCertificateAuthorities.html ListCertificateAuthorities> action. This must have the following form:
--
-- @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @ .
--
-- /Note:/ Consider using 'certificateAuthorityARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpCertificateAuthorityARN :: Lens.Lens' CreatePermission Lude.Text
cpCertificateAuthorityARN = Lens.lens (certificateAuthorityARN :: CreatePermission -> Lude.Text) (\s a -> s {certificateAuthorityARN = a} :: CreatePermission)
{-# DEPRECATED cpCertificateAuthorityARN "Use generic-lens or generic-optics with 'certificateAuthorityARN' instead." #-}

instance Lude.AWSRequest CreatePermission where
  type Rs CreatePermission = CreatePermissionResponse
  request = Req.postJSON certificateManagerPCAService
  response = Res.receiveNull CreatePermissionResponse'

instance Lude.ToHeaders CreatePermission where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("ACMPrivateCA.CreatePermission" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreatePermission where
  toJSON CreatePermission' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SourceAccount" Lude..=) Lude.<$> sourceAccount,
            Lude.Just ("Actions" Lude..= actions),
            Lude.Just ("Principal" Lude..= principal),
            Lude.Just
              ("CertificateAuthorityArn" Lude..= certificateAuthorityARN)
          ]
      )

instance Lude.ToPath CreatePermission where
  toPath = Lude.const "/"

instance Lude.ToQuery CreatePermission where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreatePermissionResponse' smart constructor.
data CreatePermissionResponse = CreatePermissionResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreatePermissionResponse' with the minimum fields required to make a request.
mkCreatePermissionResponse ::
  CreatePermissionResponse
mkCreatePermissionResponse = CreatePermissionResponse'

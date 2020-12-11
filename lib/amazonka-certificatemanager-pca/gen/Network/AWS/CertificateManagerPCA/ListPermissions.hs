{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManagerPCA.ListPermissions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List all permissions on a private CA, if any, granted to the AWS Certificate Manager (ACM) service principal (acm.amazonaws.com).
--
-- These permissions allow ACM to issue and renew ACM certificates that reside in the same AWS account as the CA.
-- Permissions can be granted with the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreatePermission.html CreatePermission> action and revoked with the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_DeletePermission.html DeletePermission> action.
-- __About Permissions__
--
--     * If the private CA and the certificates it issues reside in the same account, you can use @CreatePermission@ to grant permissions for ACM to carry out automatic certificate renewals.
--
--
--     * For automatic certificate renewal to succeed, the ACM service principal needs permissions to create, retrieve, and list certificates.
--
--
--     * If the private CA and the ACM certificates reside in different accounts, then permissions cannot be used to enable automatic renewals. Instead, the ACM certificate owner must set up a resource-based policy to enable cross-account issuance and renewals. For more information, see <acm-pca/latest/userguide/pca-rbp.html Using a Resource Based Policy with ACM Private CA> .
--
--
--
-- This operation returns paginated results.
module Network.AWS.CertificateManagerPCA.ListPermissions
  ( -- * Creating a request
    ListPermissions (..),
    mkListPermissions,

    -- ** Request lenses
    lpNextToken,
    lpMaxResults,
    lpCertificateAuthorityARN,

    -- * Destructuring the response
    ListPermissionsResponse (..),
    mkListPermissionsResponse,

    -- ** Response lenses
    lprsNextToken,
    lprsPermissions,
    lprsResponseStatus,
  )
where

import Network.AWS.CertificateManagerPCA.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListPermissions' smart constructor.
data ListPermissions = ListPermissions'
  { nextToken ::
      Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural,
    certificateAuthorityARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListPermissions' with the minimum fields required to make a request.
--
-- * 'certificateAuthorityARN' - The Amazon Resource Number (ARN) of the private CA to inspect. You can find the ARN by calling the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_ListCertificateAuthorities.html ListCertificateAuthorities> action. This must be of the form: @arn:aws:acm-pca:region:account:certificate-authority/12345678-1234-1234-1234-123456789012@ You can get a private CA's ARN by running the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_ListCertificateAuthorities.html ListCertificateAuthorities> action.
-- * 'maxResults' - When paginating results, use this parameter to specify the maximum number of items to return in the response. If additional items exist beyond the number you specify, the __NextToken__ element is sent in the response. Use this __NextToken__ value in a subsequent request to retrieve additional items.
-- * 'nextToken' - When paginating results, use this parameter in a subsequent request after you receive a response with truncated results. Set it to the value of __NextToken__ from the response you just received.
mkListPermissions ::
  -- | 'certificateAuthorityARN'
  Lude.Text ->
  ListPermissions
mkListPermissions pCertificateAuthorityARN_ =
  ListPermissions'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      certificateAuthorityARN = pCertificateAuthorityARN_
    }

-- | When paginating results, use this parameter in a subsequent request after you receive a response with truncated results. Set it to the value of __NextToken__ from the response you just received.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpNextToken :: Lens.Lens' ListPermissions (Lude.Maybe Lude.Text)
lpNextToken = Lens.lens (nextToken :: ListPermissions -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListPermissions)
{-# DEPRECATED lpNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | When paginating results, use this parameter to specify the maximum number of items to return in the response. If additional items exist beyond the number you specify, the __NextToken__ element is sent in the response. Use this __NextToken__ value in a subsequent request to retrieve additional items.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpMaxResults :: Lens.Lens' ListPermissions (Lude.Maybe Lude.Natural)
lpMaxResults = Lens.lens (maxResults :: ListPermissions -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListPermissions)
{-# DEPRECATED lpMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The Amazon Resource Number (ARN) of the private CA to inspect. You can find the ARN by calling the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_ListCertificateAuthorities.html ListCertificateAuthorities> action. This must be of the form: @arn:aws:acm-pca:region:account:certificate-authority/12345678-1234-1234-1234-123456789012@ You can get a private CA's ARN by running the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_ListCertificateAuthorities.html ListCertificateAuthorities> action.
--
-- /Note:/ Consider using 'certificateAuthorityARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpCertificateAuthorityARN :: Lens.Lens' ListPermissions Lude.Text
lpCertificateAuthorityARN = Lens.lens (certificateAuthorityARN :: ListPermissions -> Lude.Text) (\s a -> s {certificateAuthorityARN = a} :: ListPermissions)
{-# DEPRECATED lpCertificateAuthorityARN "Use generic-lens or generic-optics with 'certificateAuthorityARN' instead." #-}

instance Page.AWSPager ListPermissions where
  page rq rs
    | Page.stop (rs Lens.^. lprsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lprsPermissions) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lpNextToken Lens..~ rs Lens.^. lprsNextToken

instance Lude.AWSRequest ListPermissions where
  type Rs ListPermissions = ListPermissionsResponse
  request = Req.postJSON certificateManagerPCAService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListPermissionsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "Permissions" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListPermissions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("ACMPrivateCA.ListPermissions" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListPermissions where
  toJSON ListPermissions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just
              ("CertificateAuthorityArn" Lude..= certificateAuthorityARN)
          ]
      )

instance Lude.ToPath ListPermissions where
  toPath = Lude.const "/"

instance Lude.ToQuery ListPermissions where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListPermissionsResponse' smart constructor.
data ListPermissionsResponse = ListPermissionsResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    permissions :: Lude.Maybe [Permission],
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListPermissionsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - When the list is truncated, this value is present and should be used for the __NextToken__ parameter in a subsequent pagination request.
-- * 'permissions' - Summary information about each permission assigned by the specified private CA, including the action enabled, the policy provided, and the time of creation.
-- * 'responseStatus' - The response status code.
mkListPermissionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListPermissionsResponse
mkListPermissionsResponse pResponseStatus_ =
  ListPermissionsResponse'
    { nextToken = Lude.Nothing,
      permissions = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | When the list is truncated, this value is present and should be used for the __NextToken__ parameter in a subsequent pagination request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprsNextToken :: Lens.Lens' ListPermissionsResponse (Lude.Maybe Lude.Text)
lprsNextToken = Lens.lens (nextToken :: ListPermissionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListPermissionsResponse)
{-# DEPRECATED lprsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Summary information about each permission assigned by the specified private CA, including the action enabled, the policy provided, and the time of creation.
--
-- /Note:/ Consider using 'permissions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprsPermissions :: Lens.Lens' ListPermissionsResponse (Lude.Maybe [Permission])
lprsPermissions = Lens.lens (permissions :: ListPermissionsResponse -> Lude.Maybe [Permission]) (\s a -> s {permissions = a} :: ListPermissionsResponse)
{-# DEPRECATED lprsPermissions "Use generic-lens or generic-optics with 'permissions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprsResponseStatus :: Lens.Lens' ListPermissionsResponse Lude.Int
lprsResponseStatus = Lens.lens (responseStatus :: ListPermissionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListPermissionsResponse)
{-# DEPRECATED lprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

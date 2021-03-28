{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      ListPermissions (..)
    , mkListPermissions
    -- ** Request lenses
    , lpCertificateAuthorityArn
    , lpMaxResults
    , lpNextToken

    -- * Destructuring the response
    , ListPermissionsResponse (..)
    , mkListPermissionsResponse
    -- ** Response lenses
    , lprrsNextToken
    , lprrsPermissions
    , lprrsResponseStatus
    ) where

import qualified Network.AWS.CertificateManagerPCA.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListPermissions' smart constructor.
data ListPermissions = ListPermissions'
  { certificateAuthorityArn :: Types.Arn
    -- ^ The Amazon Resource Number (ARN) of the private CA to inspect. You can find the ARN by calling the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_ListCertificateAuthorities.html ListCertificateAuthorities> action. This must be of the form: @arn:aws:acm-pca:region:account:certificate-authority/12345678-1234-1234-1234-123456789012@ You can get a private CA's ARN by running the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_ListCertificateAuthorities.html ListCertificateAuthorities> action.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ When paginating results, use this parameter to specify the maximum number of items to return in the response. If additional items exist beyond the number you specify, the __NextToken__ element is sent in the response. Use this __NextToken__ value in a subsequent request to retrieve additional items.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ When paginating results, use this parameter in a subsequent request after you receive a response with truncated results. Set it to the value of __NextToken__ from the response you just received.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListPermissions' value with any optional fields omitted.
mkListPermissions
    :: Types.Arn -- ^ 'certificateAuthorityArn'
    -> ListPermissions
mkListPermissions certificateAuthorityArn
  = ListPermissions'{certificateAuthorityArn,
                     maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | The Amazon Resource Number (ARN) of the private CA to inspect. You can find the ARN by calling the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_ListCertificateAuthorities.html ListCertificateAuthorities> action. This must be of the form: @arn:aws:acm-pca:region:account:certificate-authority/12345678-1234-1234-1234-123456789012@ You can get a private CA's ARN by running the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_ListCertificateAuthorities.html ListCertificateAuthorities> action.
--
-- /Note:/ Consider using 'certificateAuthorityArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpCertificateAuthorityArn :: Lens.Lens' ListPermissions Types.Arn
lpCertificateAuthorityArn = Lens.field @"certificateAuthorityArn"
{-# INLINEABLE lpCertificateAuthorityArn #-}
{-# DEPRECATED certificateAuthorityArn "Use generic-lens or generic-optics with 'certificateAuthorityArn' instead"  #-}

-- | When paginating results, use this parameter to specify the maximum number of items to return in the response. If additional items exist beyond the number you specify, the __NextToken__ element is sent in the response. Use this __NextToken__ value in a subsequent request to retrieve additional items.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpMaxResults :: Lens.Lens' ListPermissions (Core.Maybe Core.Natural)
lpMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lpMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | When paginating results, use this parameter in a subsequent request after you receive a response with truncated results. Set it to the value of __NextToken__ from the response you just received.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpNextToken :: Lens.Lens' ListPermissions (Core.Maybe Types.NextToken)
lpNextToken = Lens.field @"nextToken"
{-# INLINEABLE lpNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListPermissions where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListPermissions where
        toHeaders ListPermissions{..}
          = Core.pure ("X-Amz-Target", "ACMPrivateCA.ListPermissions")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListPermissions where
        toJSON ListPermissions{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("CertificateAuthorityArn" Core..= certificateAuthorityArn),
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListPermissions where
        type Rs ListPermissions = ListPermissionsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListPermissionsResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*> x Core..:? "Permissions" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListPermissions where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"permissions" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListPermissionsResponse' smart constructor.
data ListPermissionsResponse = ListPermissionsResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ When the list is truncated, this value is present and should be used for the __NextToken__ parameter in a subsequent pagination request. 
  , permissions :: Core.Maybe [Types.Permission]
    -- ^ Summary information about each permission assigned by the specified private CA, including the action enabled, the policy provided, and the time of creation.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListPermissionsResponse' value with any optional fields omitted.
mkListPermissionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListPermissionsResponse
mkListPermissionsResponse responseStatus
  = ListPermissionsResponse'{nextToken = Core.Nothing,
                             permissions = Core.Nothing, responseStatus}

-- | When the list is truncated, this value is present and should be used for the __NextToken__ parameter in a subsequent pagination request. 
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprrsNextToken :: Lens.Lens' ListPermissionsResponse (Core.Maybe Types.NextToken)
lprrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lprrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Summary information about each permission assigned by the specified private CA, including the action enabled, the policy provided, and the time of creation.
--
-- /Note:/ Consider using 'permissions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprrsPermissions :: Lens.Lens' ListPermissionsResponse (Core.Maybe [Types.Permission])
lprrsPermissions = Lens.field @"permissions"
{-# INLINEABLE lprrsPermissions #-}
{-# DEPRECATED permissions "Use generic-lens or generic-optics with 'permissions' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprrsResponseStatus :: Lens.Lens' ListPermissionsResponse Core.Int
lprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

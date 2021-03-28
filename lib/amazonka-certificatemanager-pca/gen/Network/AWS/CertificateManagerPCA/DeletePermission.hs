{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
--
--
module Network.AWS.CertificateManagerPCA.DeletePermission
    (
    -- * Creating a request
      DeletePermission (..)
    , mkDeletePermission
    -- ** Request lenses
    , dpCertificateAuthorityArn
    , dpPrincipal
    , dpSourceAccount

    -- * Destructuring the response
    , DeletePermissionResponse (..)
    , mkDeletePermissionResponse
    ) where

import qualified Network.AWS.CertificateManagerPCA.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeletePermission' smart constructor.
data DeletePermission = DeletePermission'
  { certificateAuthorityArn :: Types.Arn
    -- ^ The Amazon Resource Number (ARN) of the private CA that issued the permissions. You can find the CA's ARN by calling the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_ListCertificateAuthorities.html ListCertificateAuthorities> action. This must have the following form: 
--
-- @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @ . 
  , principal :: Types.Principal
    -- ^ The AWS service or identity that will have its CA permissions revoked. At this time, the only valid service principal is @acm.amazonaws.com@ 
  , sourceAccount :: Core.Maybe Types.AccountId
    -- ^ The AWS account that calls this action.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeletePermission' value with any optional fields omitted.
mkDeletePermission
    :: Types.Arn -- ^ 'certificateAuthorityArn'
    -> Types.Principal -- ^ 'principal'
    -> DeletePermission
mkDeletePermission certificateAuthorityArn principal
  = DeletePermission'{certificateAuthorityArn, principal,
                      sourceAccount = Core.Nothing}

-- | The Amazon Resource Number (ARN) of the private CA that issued the permissions. You can find the CA's ARN by calling the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_ListCertificateAuthorities.html ListCertificateAuthorities> action. This must have the following form: 
--
-- @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @ . 
--
-- /Note:/ Consider using 'certificateAuthorityArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpCertificateAuthorityArn :: Lens.Lens' DeletePermission Types.Arn
dpCertificateAuthorityArn = Lens.field @"certificateAuthorityArn"
{-# INLINEABLE dpCertificateAuthorityArn #-}
{-# DEPRECATED certificateAuthorityArn "Use generic-lens or generic-optics with 'certificateAuthorityArn' instead"  #-}

-- | The AWS service or identity that will have its CA permissions revoked. At this time, the only valid service principal is @acm.amazonaws.com@ 
--
-- /Note:/ Consider using 'principal' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpPrincipal :: Lens.Lens' DeletePermission Types.Principal
dpPrincipal = Lens.field @"principal"
{-# INLINEABLE dpPrincipal #-}
{-# DEPRECATED principal "Use generic-lens or generic-optics with 'principal' instead"  #-}

-- | The AWS account that calls this action.
--
-- /Note:/ Consider using 'sourceAccount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpSourceAccount :: Lens.Lens' DeletePermission (Core.Maybe Types.AccountId)
dpSourceAccount = Lens.field @"sourceAccount"
{-# INLINEABLE dpSourceAccount #-}
{-# DEPRECATED sourceAccount "Use generic-lens or generic-optics with 'sourceAccount' instead"  #-}

instance Core.ToQuery DeletePermission where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeletePermission where
        toHeaders DeletePermission{..}
          = Core.pure ("X-Amz-Target", "ACMPrivateCA.DeletePermission")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeletePermission where
        toJSON DeletePermission{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("CertificateAuthorityArn" Core..= certificateAuthorityArn),
                  Core.Just ("Principal" Core..= principal),
                  ("SourceAccount" Core..=) Core.<$> sourceAccount])

instance Core.AWSRequest DeletePermission where
        type Rs DeletePermission = DeletePermissionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull DeletePermissionResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeletePermissionResponse' smart constructor.
data DeletePermissionResponse = DeletePermissionResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeletePermissionResponse' value with any optional fields omitted.
mkDeletePermissionResponse
    :: DeletePermissionResponse
mkDeletePermissionResponse = DeletePermissionResponse'

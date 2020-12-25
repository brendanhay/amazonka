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
    cpCertificateAuthorityArn,
    cpPrincipal,
    cpActions,
    cpSourceAccount,

    -- * Destructuring the response
    CreatePermissionResponse (..),
    mkCreatePermissionResponse,
  )
where

import qualified Network.AWS.CertificateManagerPCA.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreatePermission' smart constructor.
data CreatePermission = CreatePermission'
  { -- | The Amazon Resource Name (ARN) of the CA that grants the permissions. You can find the ARN by calling the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_ListCertificateAuthorities.html ListCertificateAuthorities> action. This must have the following form:
    --
    -- @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @ .
    certificateAuthorityArn :: Types.Arn,
    -- | The AWS service or identity that receives the permission. At this time, the only valid principal is @acm.amazonaws.com@ .
    principal :: Types.Principal,
    -- | The actions that the specified AWS service principal can use. These include @IssueCertificate@ , @GetCertificate@ , and @ListPermissions@ .
    actions :: Core.NonEmpty Types.ActionType,
    -- | The ID of the calling account.
    sourceAccount :: Core.Maybe Types.AccountId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreatePermission' value with any optional fields omitted.
mkCreatePermission ::
  -- | 'certificateAuthorityArn'
  Types.Arn ->
  -- | 'principal'
  Types.Principal ->
  -- | 'actions'
  Core.NonEmpty Types.ActionType ->
  CreatePermission
mkCreatePermission certificateAuthorityArn principal actions =
  CreatePermission'
    { certificateAuthorityArn,
      principal,
      actions,
      sourceAccount = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the CA that grants the permissions. You can find the ARN by calling the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_ListCertificateAuthorities.html ListCertificateAuthorities> action. This must have the following form:
--
-- @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @ .
--
-- /Note:/ Consider using 'certificateAuthorityArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpCertificateAuthorityArn :: Lens.Lens' CreatePermission Types.Arn
cpCertificateAuthorityArn = Lens.field @"certificateAuthorityArn"
{-# DEPRECATED cpCertificateAuthorityArn "Use generic-lens or generic-optics with 'certificateAuthorityArn' instead." #-}

-- | The AWS service or identity that receives the permission. At this time, the only valid principal is @acm.amazonaws.com@ .
--
-- /Note:/ Consider using 'principal' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpPrincipal :: Lens.Lens' CreatePermission Types.Principal
cpPrincipal = Lens.field @"principal"
{-# DEPRECATED cpPrincipal "Use generic-lens or generic-optics with 'principal' instead." #-}

-- | The actions that the specified AWS service principal can use. These include @IssueCertificate@ , @GetCertificate@ , and @ListPermissions@ .
--
-- /Note:/ Consider using 'actions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpActions :: Lens.Lens' CreatePermission (Core.NonEmpty Types.ActionType)
cpActions = Lens.field @"actions"
{-# DEPRECATED cpActions "Use generic-lens or generic-optics with 'actions' instead." #-}

-- | The ID of the calling account.
--
-- /Note:/ Consider using 'sourceAccount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpSourceAccount :: Lens.Lens' CreatePermission (Core.Maybe Types.AccountId)
cpSourceAccount = Lens.field @"sourceAccount"
{-# DEPRECATED cpSourceAccount "Use generic-lens or generic-optics with 'sourceAccount' instead." #-}

instance Core.FromJSON CreatePermission where
  toJSON CreatePermission {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("CertificateAuthorityArn" Core..= certificateAuthorityArn),
            Core.Just ("Principal" Core..= principal),
            Core.Just ("Actions" Core..= actions),
            ("SourceAccount" Core..=) Core.<$> sourceAccount
          ]
      )

instance Core.AWSRequest CreatePermission where
  type Rs CreatePermission = CreatePermissionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "ACMPrivateCA.CreatePermission")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull CreatePermissionResponse'

-- | /See:/ 'mkCreatePermissionResponse' smart constructor.
data CreatePermissionResponse = CreatePermissionResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreatePermissionResponse' value with any optional fields omitted.
mkCreatePermissionResponse ::
  CreatePermissionResponse
mkCreatePermissionResponse = CreatePermissionResponse'

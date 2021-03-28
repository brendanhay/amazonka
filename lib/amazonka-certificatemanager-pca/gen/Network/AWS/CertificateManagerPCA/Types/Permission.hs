{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManagerPCA.Types.Permission
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CertificateManagerPCA.Types.Permission
  ( Permission (..)
  -- * Smart constructor
  , mkPermission
  -- * Lenses
  , pActions
  , pCertificateAuthorityArn
  , pCreatedAt
  , pPolicy
  , pPrincipal
  , pSourceAccount
  ) where

import qualified Network.AWS.CertificateManagerPCA.Types.AWSPolicy as Types
import qualified Network.AWS.CertificateManagerPCA.Types.AccountId as Types
import qualified Network.AWS.CertificateManagerPCA.Types.ActionType as Types
import qualified Network.AWS.CertificateManagerPCA.Types.Arn as Types
import qualified Network.AWS.CertificateManagerPCA.Types.Principal as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Permissions designate which private CA actions can be performed by an AWS service or entity. In order for ACM to automatically renew private certificates, you must give the ACM service principal all available permissions (@IssueCertificate@ , @GetCertificate@ , and @ListPermissions@ ). Permissions can be assigned with the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreatePermission.html CreatePermission> action, removed with the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_DeletePermission.html DeletePermission> action, and listed with the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_ListPermissions.html ListPermissions> action.
--
-- /See:/ 'mkPermission' smart constructor.
data Permission = Permission'
  { actions :: Core.Maybe (Core.NonEmpty Types.ActionType)
    -- ^ The private CA actions that can be performed by the designated AWS service.
  , certificateAuthorityArn :: Core.Maybe Types.Arn
    -- ^ The Amazon Resource Number (ARN) of the private CA from which the permission was issued.
  , createdAt :: Core.Maybe Core.NominalDiffTime
    -- ^ The time at which the permission was created.
  , policy :: Core.Maybe Types.AWSPolicy
    -- ^ The name of the policy that is associated with the permission.
  , principal :: Core.Maybe Types.Principal
    -- ^ The AWS service or entity that holds the permission. At this time, the only valid principal is @acm.amazonaws.com@ .
  , sourceAccount :: Core.Maybe Types.AccountId
    -- ^ The ID of the account that assigned the permission.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'Permission' value with any optional fields omitted.
mkPermission
    :: Permission
mkPermission
  = Permission'{actions = Core.Nothing,
                certificateAuthorityArn = Core.Nothing, createdAt = Core.Nothing,
                policy = Core.Nothing, principal = Core.Nothing,
                sourceAccount = Core.Nothing}

-- | The private CA actions that can be performed by the designated AWS service.
--
-- /Note:/ Consider using 'actions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pActions :: Lens.Lens' Permission (Core.Maybe (Core.NonEmpty Types.ActionType))
pActions = Lens.field @"actions"
{-# INLINEABLE pActions #-}
{-# DEPRECATED actions "Use generic-lens or generic-optics with 'actions' instead"  #-}

-- | The Amazon Resource Number (ARN) of the private CA from which the permission was issued.
--
-- /Note:/ Consider using 'certificateAuthorityArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pCertificateAuthorityArn :: Lens.Lens' Permission (Core.Maybe Types.Arn)
pCertificateAuthorityArn = Lens.field @"certificateAuthorityArn"
{-# INLINEABLE pCertificateAuthorityArn #-}
{-# DEPRECATED certificateAuthorityArn "Use generic-lens or generic-optics with 'certificateAuthorityArn' instead"  #-}

-- | The time at which the permission was created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pCreatedAt :: Lens.Lens' Permission (Core.Maybe Core.NominalDiffTime)
pCreatedAt = Lens.field @"createdAt"
{-# INLINEABLE pCreatedAt #-}
{-# DEPRECATED createdAt "Use generic-lens or generic-optics with 'createdAt' instead"  #-}

-- | The name of the policy that is associated with the permission.
--
-- /Note:/ Consider using 'policy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pPolicy :: Lens.Lens' Permission (Core.Maybe Types.AWSPolicy)
pPolicy = Lens.field @"policy"
{-# INLINEABLE pPolicy #-}
{-# DEPRECATED policy "Use generic-lens or generic-optics with 'policy' instead"  #-}

-- | The AWS service or entity that holds the permission. At this time, the only valid principal is @acm.amazonaws.com@ .
--
-- /Note:/ Consider using 'principal' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pPrincipal :: Lens.Lens' Permission (Core.Maybe Types.Principal)
pPrincipal = Lens.field @"principal"
{-# INLINEABLE pPrincipal #-}
{-# DEPRECATED principal "Use generic-lens or generic-optics with 'principal' instead"  #-}

-- | The ID of the account that assigned the permission.
--
-- /Note:/ Consider using 'sourceAccount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pSourceAccount :: Lens.Lens' Permission (Core.Maybe Types.AccountId)
pSourceAccount = Lens.field @"sourceAccount"
{-# INLINEABLE pSourceAccount #-}
{-# DEPRECATED sourceAccount "Use generic-lens or generic-optics with 'sourceAccount' instead"  #-}

instance Core.FromJSON Permission where
        parseJSON
          = Core.withObject "Permission" Core.$
              \ x ->
                Permission' Core.<$>
                  (x Core..:? "Actions") Core.<*>
                    x Core..:? "CertificateAuthorityArn"
                    Core.<*> x Core..:? "CreatedAt"
                    Core.<*> x Core..:? "Policy"
                    Core.<*> x Core..:? "Principal"
                    Core.<*> x Core..:? "SourceAccount"

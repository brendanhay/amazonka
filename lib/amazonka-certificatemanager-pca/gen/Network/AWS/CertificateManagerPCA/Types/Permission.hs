{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManagerPCA.Types.Permission
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManagerPCA.Types.Permission
  ( Permission (..),

    -- * Smart constructor
    mkPermission,

    -- * Lenses
    pSourceAccount,
    pActions,
    pCreatedAt,
    pPrincipal,
    pPolicy,
    pCertificateAuthorityARN,
  )
where

import Network.AWS.CertificateManagerPCA.Types.ActionType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Permissions designate which private CA actions can be performed by an AWS service or entity. In order for ACM to automatically renew private certificates, you must give the ACM service principal all available permissions (@IssueCertificate@ , @GetCertificate@ , and @ListPermissions@ ). Permissions can be assigned with the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreatePermission.html CreatePermission> action, removed with the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_DeletePermission.html DeletePermission> action, and listed with the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_ListPermissions.html ListPermissions> action.
--
-- /See:/ 'mkPermission' smart constructor.
data Permission = Permission'
  { sourceAccount ::
      Lude.Maybe Lude.Text,
    actions :: Lude.Maybe (Lude.NonEmpty ActionType),
    createdAt :: Lude.Maybe Lude.Timestamp,
    principal :: Lude.Maybe Lude.Text,
    policy :: Lude.Maybe Lude.Text,
    certificateAuthorityARN :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Permission' with the minimum fields required to make a request.
--
-- * 'actions' - The private CA actions that can be performed by the designated AWS service.
-- * 'certificateAuthorityARN' - The Amazon Resource Number (ARN) of the private CA from which the permission was issued.
-- * 'createdAt' - The time at which the permission was created.
-- * 'policy' - The name of the policy that is associated with the permission.
-- * 'principal' - The AWS service or entity that holds the permission. At this time, the only valid principal is @acm.amazonaws.com@ .
-- * 'sourceAccount' - The ID of the account that assigned the permission.
mkPermission ::
  Permission
mkPermission =
  Permission'
    { sourceAccount = Lude.Nothing,
      actions = Lude.Nothing,
      createdAt = Lude.Nothing,
      principal = Lude.Nothing,
      policy = Lude.Nothing,
      certificateAuthorityARN = Lude.Nothing
    }

-- | The ID of the account that assigned the permission.
--
-- /Note:/ Consider using 'sourceAccount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pSourceAccount :: Lens.Lens' Permission (Lude.Maybe Lude.Text)
pSourceAccount = Lens.lens (sourceAccount :: Permission -> Lude.Maybe Lude.Text) (\s a -> s {sourceAccount = a} :: Permission)
{-# DEPRECATED pSourceAccount "Use generic-lens or generic-optics with 'sourceAccount' instead." #-}

-- | The private CA actions that can be performed by the designated AWS service.
--
-- /Note:/ Consider using 'actions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pActions :: Lens.Lens' Permission (Lude.Maybe (Lude.NonEmpty ActionType))
pActions = Lens.lens (actions :: Permission -> Lude.Maybe (Lude.NonEmpty ActionType)) (\s a -> s {actions = a} :: Permission)
{-# DEPRECATED pActions "Use generic-lens or generic-optics with 'actions' instead." #-}

-- | The time at which the permission was created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pCreatedAt :: Lens.Lens' Permission (Lude.Maybe Lude.Timestamp)
pCreatedAt = Lens.lens (createdAt :: Permission -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdAt = a} :: Permission)
{-# DEPRECATED pCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | The AWS service or entity that holds the permission. At this time, the only valid principal is @acm.amazonaws.com@ .
--
-- /Note:/ Consider using 'principal' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pPrincipal :: Lens.Lens' Permission (Lude.Maybe Lude.Text)
pPrincipal = Lens.lens (principal :: Permission -> Lude.Maybe Lude.Text) (\s a -> s {principal = a} :: Permission)
{-# DEPRECATED pPrincipal "Use generic-lens or generic-optics with 'principal' instead." #-}

-- | The name of the policy that is associated with the permission.
--
-- /Note:/ Consider using 'policy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pPolicy :: Lens.Lens' Permission (Lude.Maybe Lude.Text)
pPolicy = Lens.lens (policy :: Permission -> Lude.Maybe Lude.Text) (\s a -> s {policy = a} :: Permission)
{-# DEPRECATED pPolicy "Use generic-lens or generic-optics with 'policy' instead." #-}

-- | The Amazon Resource Number (ARN) of the private CA from which the permission was issued.
--
-- /Note:/ Consider using 'certificateAuthorityARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pCertificateAuthorityARN :: Lens.Lens' Permission (Lude.Maybe Lude.Text)
pCertificateAuthorityARN = Lens.lens (certificateAuthorityARN :: Permission -> Lude.Maybe Lude.Text) (\s a -> s {certificateAuthorityARN = a} :: Permission)
{-# DEPRECATED pCertificateAuthorityARN "Use generic-lens or generic-optics with 'certificateAuthorityARN' instead." #-}

instance Lude.FromJSON Permission where
  parseJSON =
    Lude.withObject
      "Permission"
      ( \x ->
          Permission'
            Lude.<$> (x Lude..:? "SourceAccount")
            Lude.<*> (x Lude..:? "Actions")
            Lude.<*> (x Lude..:? "CreatedAt")
            Lude.<*> (x Lude..:? "Principal")
            Lude.<*> (x Lude..:? "Policy")
            Lude.<*> (x Lude..:? "CertificateAuthorityArn")
      )

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.PublicAccess
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.PublicAccess
  ( PublicAccess (..),

    -- * Smart constructor
    mkPublicAccess,

    -- * Lenses
    paPermissionConfiguration,
    paEffectivePermission,
  )
where

import Network.AWS.GuardDuty.Types.PermissionConfiguration
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the public access policies that apply to the S3 bucket.
--
-- /See:/ 'mkPublicAccess' smart constructor.
data PublicAccess = PublicAccess'
  { -- | Contains information about how permissions are configured for the S3 bucket.
    permissionConfiguration :: Lude.Maybe PermissionConfiguration,
    -- | Describes the effective permission on this bucket after factoring all attached policies.
    effectivePermission :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PublicAccess' with the minimum fields required to make a request.
--
-- * 'permissionConfiguration' - Contains information about how permissions are configured for the S3 bucket.
-- * 'effectivePermission' - Describes the effective permission on this bucket after factoring all attached policies.
mkPublicAccess ::
  PublicAccess
mkPublicAccess =
  PublicAccess'
    { permissionConfiguration = Lude.Nothing,
      effectivePermission = Lude.Nothing
    }

-- | Contains information about how permissions are configured for the S3 bucket.
--
-- /Note:/ Consider using 'permissionConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paPermissionConfiguration :: Lens.Lens' PublicAccess (Lude.Maybe PermissionConfiguration)
paPermissionConfiguration = Lens.lens (permissionConfiguration :: PublicAccess -> Lude.Maybe PermissionConfiguration) (\s a -> s {permissionConfiguration = a} :: PublicAccess)
{-# DEPRECATED paPermissionConfiguration "Use generic-lens or generic-optics with 'permissionConfiguration' instead." #-}

-- | Describes the effective permission on this bucket after factoring all attached policies.
--
-- /Note:/ Consider using 'effectivePermission' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paEffectivePermission :: Lens.Lens' PublicAccess (Lude.Maybe Lude.Text)
paEffectivePermission = Lens.lens (effectivePermission :: PublicAccess -> Lude.Maybe Lude.Text) (\s a -> s {effectivePermission = a} :: PublicAccess)
{-# DEPRECATED paEffectivePermission "Use generic-lens or generic-optics with 'effectivePermission' instead." #-}

instance Lude.FromJSON PublicAccess where
  parseJSON =
    Lude.withObject
      "PublicAccess"
      ( \x ->
          PublicAccess'
            Lude.<$> (x Lude..:? "permissionConfiguration")
            Lude.<*> (x Lude..:? "effectivePermission")
      )

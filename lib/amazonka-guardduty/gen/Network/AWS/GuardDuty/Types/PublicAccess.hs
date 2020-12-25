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
    paEffectivePermission,
    paPermissionConfiguration,
  )
where

import qualified Network.AWS.GuardDuty.Types.PermissionConfiguration as Types
import qualified Network.AWS.GuardDuty.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the public access policies that apply to the S3 bucket.
--
-- /See:/ 'mkPublicAccess' smart constructor.
data PublicAccess = PublicAccess'
  { -- | Describes the effective permission on this bucket after factoring all attached policies.
    effectivePermission :: Core.Maybe Types.String,
    -- | Contains information about how permissions are configured for the S3 bucket.
    permissionConfiguration :: Core.Maybe Types.PermissionConfiguration
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PublicAccess' value with any optional fields omitted.
mkPublicAccess ::
  PublicAccess
mkPublicAccess =
  PublicAccess'
    { effectivePermission = Core.Nothing,
      permissionConfiguration = Core.Nothing
    }

-- | Describes the effective permission on this bucket after factoring all attached policies.
--
-- /Note:/ Consider using 'effectivePermission' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paEffectivePermission :: Lens.Lens' PublicAccess (Core.Maybe Types.String)
paEffectivePermission = Lens.field @"effectivePermission"
{-# DEPRECATED paEffectivePermission "Use generic-lens or generic-optics with 'effectivePermission' instead." #-}

-- | Contains information about how permissions are configured for the S3 bucket.
--
-- /Note:/ Consider using 'permissionConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paPermissionConfiguration :: Lens.Lens' PublicAccess (Core.Maybe Types.PermissionConfiguration)
paPermissionConfiguration = Lens.field @"permissionConfiguration"
{-# DEPRECATED paPermissionConfiguration "Use generic-lens or generic-optics with 'permissionConfiguration' instead." #-}

instance Core.FromJSON PublicAccess where
  parseJSON =
    Core.withObject "PublicAccess" Core.$
      \x ->
        PublicAccess'
          Core.<$> (x Core..:? "effectivePermission")
          Core.<*> (x Core..:? "permissionConfiguration")

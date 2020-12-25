{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.Principal
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.Principal
  ( Principal (..),

    -- * Smart constructor
    mkPrincipal,

    -- * Lenses
    pPrincipalARN,
    pPrincipalType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.ServiceCatalog.Types.PrincipalARN as Types
import qualified Network.AWS.ServiceCatalog.Types.PrincipalType as Types

-- | Information about a principal.
--
-- /See:/ 'mkPrincipal' smart constructor.
data Principal = Principal'
  { -- | The ARN of the principal (IAM user, role, or group).
    principalARN :: Core.Maybe Types.PrincipalARN,
    -- | The principal type. The supported value is @IAM@ .
    principalType :: Core.Maybe Types.PrincipalType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Principal' value with any optional fields omitted.
mkPrincipal ::
  Principal
mkPrincipal =
  Principal'
    { principalARN = Core.Nothing,
      principalType = Core.Nothing
    }

-- | The ARN of the principal (IAM user, role, or group).
--
-- /Note:/ Consider using 'principalARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pPrincipalARN :: Lens.Lens' Principal (Core.Maybe Types.PrincipalARN)
pPrincipalARN = Lens.field @"principalARN"
{-# DEPRECATED pPrincipalARN "Use generic-lens or generic-optics with 'principalARN' instead." #-}

-- | The principal type. The supported value is @IAM@ .
--
-- /Note:/ Consider using 'principalType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pPrincipalType :: Lens.Lens' Principal (Core.Maybe Types.PrincipalType)
pPrincipalType = Lens.field @"principalType"
{-# DEPRECATED pPrincipalType "Use generic-lens or generic-optics with 'principalType' instead." #-}

instance Core.FromJSON Principal where
  parseJSON =
    Core.withObject "Principal" Core.$
      \x ->
        Principal'
          Core.<$> (x Core..:? "PrincipalARN") Core.<*> (x Core..:? "PrincipalType")

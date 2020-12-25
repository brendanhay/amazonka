{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.PermissionsBoundaryDecisionDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.PermissionsBoundaryDecisionDetail
  ( PermissionsBoundaryDecisionDetail (..),

    -- * Smart constructor
    mkPermissionsBoundaryDecisionDetail,

    -- * Lenses
    pbddAllowedByPermissionsBoundary,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about the effect that a permissions boundary has on a policy simulation when the boundary is applied to an IAM entity.
--
-- /See:/ 'mkPermissionsBoundaryDecisionDetail' smart constructor.
newtype PermissionsBoundaryDecisionDetail = PermissionsBoundaryDecisionDetail'
  { -- | Specifies whether an action is allowed by a permissions boundary that is applied to an IAM entity (user or role). A value of @true@ means that the permissions boundary does not deny the action. This means that the policy includes an @Allow@ statement that matches the request. In this case, if an identity-based policy also allows the action, the request is allowed. A value of @false@ means that either the requested action is not allowed (implicitly denied) or that the action is explicitly denied by the permissions boundary. In both of these cases, the action is not allowed, regardless of the identity-based policy.
    allowedByPermissionsBoundary :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'PermissionsBoundaryDecisionDetail' value with any optional fields omitted.
mkPermissionsBoundaryDecisionDetail ::
  PermissionsBoundaryDecisionDetail
mkPermissionsBoundaryDecisionDetail =
  PermissionsBoundaryDecisionDetail'
    { allowedByPermissionsBoundary =
        Core.Nothing
    }

-- | Specifies whether an action is allowed by a permissions boundary that is applied to an IAM entity (user or role). A value of @true@ means that the permissions boundary does not deny the action. This means that the policy includes an @Allow@ statement that matches the request. In this case, if an identity-based policy also allows the action, the request is allowed. A value of @false@ means that either the requested action is not allowed (implicitly denied) or that the action is explicitly denied by the permissions boundary. In both of these cases, the action is not allowed, regardless of the identity-based policy.
--
-- /Note:/ Consider using 'allowedByPermissionsBoundary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbddAllowedByPermissionsBoundary :: Lens.Lens' PermissionsBoundaryDecisionDetail (Core.Maybe Core.Bool)
pbddAllowedByPermissionsBoundary = Lens.field @"allowedByPermissionsBoundary"
{-# DEPRECATED pbddAllowedByPermissionsBoundary "Use generic-lens or generic-optics with 'allowedByPermissionsBoundary' instead." #-}

instance Core.FromXML PermissionsBoundaryDecisionDetail where
  parseXML x =
    PermissionsBoundaryDecisionDetail'
      Core.<$> (x Core..@? "AllowedByPermissionsBoundary")

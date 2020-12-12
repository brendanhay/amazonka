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
import qualified Network.AWS.Prelude as Lude

-- | Contains information about the effect that a permissions boundary has on a policy simulation when the boundary is applied to an IAM entity.
--
-- /See:/ 'mkPermissionsBoundaryDecisionDetail' smart constructor.
newtype PermissionsBoundaryDecisionDetail = PermissionsBoundaryDecisionDetail'
  { allowedByPermissionsBoundary ::
      Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PermissionsBoundaryDecisionDetail' with the minimum fields required to make a request.
--
-- * 'allowedByPermissionsBoundary' - Specifies whether an action is allowed by a permissions boundary that is applied to an IAM entity (user or role). A value of @true@ means that the permissions boundary does not deny the action. This means that the policy includes an @Allow@ statement that matches the request. In this case, if an identity-based policy also allows the action, the request is allowed. A value of @false@ means that either the requested action is not allowed (implicitly denied) or that the action is explicitly denied by the permissions boundary. In both of these cases, the action is not allowed, regardless of the identity-based policy.
mkPermissionsBoundaryDecisionDetail ::
  PermissionsBoundaryDecisionDetail
mkPermissionsBoundaryDecisionDetail =
  PermissionsBoundaryDecisionDetail'
    { allowedByPermissionsBoundary =
        Lude.Nothing
    }

-- | Specifies whether an action is allowed by a permissions boundary that is applied to an IAM entity (user or role). A value of @true@ means that the permissions boundary does not deny the action. This means that the policy includes an @Allow@ statement that matches the request. In this case, if an identity-based policy also allows the action, the request is allowed. A value of @false@ means that either the requested action is not allowed (implicitly denied) or that the action is explicitly denied by the permissions boundary. In both of these cases, the action is not allowed, regardless of the identity-based policy.
--
-- /Note:/ Consider using 'allowedByPermissionsBoundary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbddAllowedByPermissionsBoundary :: Lens.Lens' PermissionsBoundaryDecisionDetail (Lude.Maybe Lude.Bool)
pbddAllowedByPermissionsBoundary = Lens.lens (allowedByPermissionsBoundary :: PermissionsBoundaryDecisionDetail -> Lude.Maybe Lude.Bool) (\s a -> s {allowedByPermissionsBoundary = a} :: PermissionsBoundaryDecisionDetail)
{-# DEPRECATED pbddAllowedByPermissionsBoundary "Use generic-lens or generic-optics with 'allowedByPermissionsBoundary' instead." #-}

instance Lude.FromXML PermissionsBoundaryDecisionDetail where
  parseXML x =
    PermissionsBoundaryDecisionDetail'
      Lude.<$> (x Lude..@? "AllowedByPermissionsBoundary")

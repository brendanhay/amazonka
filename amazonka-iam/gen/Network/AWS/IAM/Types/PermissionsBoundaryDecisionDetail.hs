{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.PermissionsBoundaryDecisionDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.PermissionsBoundaryDecisionDetail where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains information about the effect that a permissions boundary has on
-- a policy simulation when the boundary is applied to an IAM entity.
--
-- /See:/ 'newPermissionsBoundaryDecisionDetail' smart constructor.
data PermissionsBoundaryDecisionDetail = PermissionsBoundaryDecisionDetail'
  { -- | Specifies whether an action is allowed by a permissions boundary that is
    -- applied to an IAM entity (user or role). A value of @true@ means that
    -- the permissions boundary does not deny the action. This means that the
    -- policy includes an @Allow@ statement that matches the request. In this
    -- case, if an identity-based policy also allows the action, the request is
    -- allowed. A value of @false@ means that either the requested action is
    -- not allowed (implicitly denied) or that the action is explicitly denied
    -- by the permissions boundary. In both of these cases, the action is not
    -- allowed, regardless of the identity-based policy.
    allowedByPermissionsBoundary :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PermissionsBoundaryDecisionDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowedByPermissionsBoundary', 'permissionsBoundaryDecisionDetail_allowedByPermissionsBoundary' - Specifies whether an action is allowed by a permissions boundary that is
-- applied to an IAM entity (user or role). A value of @true@ means that
-- the permissions boundary does not deny the action. This means that the
-- policy includes an @Allow@ statement that matches the request. In this
-- case, if an identity-based policy also allows the action, the request is
-- allowed. A value of @false@ means that either the requested action is
-- not allowed (implicitly denied) or that the action is explicitly denied
-- by the permissions boundary. In both of these cases, the action is not
-- allowed, regardless of the identity-based policy.
newPermissionsBoundaryDecisionDetail ::
  PermissionsBoundaryDecisionDetail
newPermissionsBoundaryDecisionDetail =
  PermissionsBoundaryDecisionDetail'
    { allowedByPermissionsBoundary =
        Prelude.Nothing
    }

-- | Specifies whether an action is allowed by a permissions boundary that is
-- applied to an IAM entity (user or role). A value of @true@ means that
-- the permissions boundary does not deny the action. This means that the
-- policy includes an @Allow@ statement that matches the request. In this
-- case, if an identity-based policy also allows the action, the request is
-- allowed. A value of @false@ means that either the requested action is
-- not allowed (implicitly denied) or that the action is explicitly denied
-- by the permissions boundary. In both of these cases, the action is not
-- allowed, regardless of the identity-based policy.
permissionsBoundaryDecisionDetail_allowedByPermissionsBoundary :: Lens.Lens' PermissionsBoundaryDecisionDetail (Prelude.Maybe Prelude.Bool)
permissionsBoundaryDecisionDetail_allowedByPermissionsBoundary = Lens.lens (\PermissionsBoundaryDecisionDetail' {allowedByPermissionsBoundary} -> allowedByPermissionsBoundary) (\s@PermissionsBoundaryDecisionDetail' {} a -> s {allowedByPermissionsBoundary = a} :: PermissionsBoundaryDecisionDetail)

instance
  Prelude.FromXML
    PermissionsBoundaryDecisionDetail
  where
  parseXML x =
    PermissionsBoundaryDecisionDetail'
      Prelude.<$> (x Prelude..@? "AllowedByPermissionsBoundary")

instance
  Prelude.Hashable
    PermissionsBoundaryDecisionDetail

instance
  Prelude.NFData
    PermissionsBoundaryDecisionDetail

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.PermissionsBoundaryDecisionDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.PermissionsBoundaryDecisionDetail where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about the effect that a permissions boundary has on a policy simulation when the boundary is applied to an IAM entity.
--
--
--
-- /See:/ 'permissionsBoundaryDecisionDetail' smart constructor.
newtype PermissionsBoundaryDecisionDetail = PermissionsBoundaryDecisionDetail'
  { _pbddAllowedByPermissionsBoundary ::
      Maybe Bool
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PermissionsBoundaryDecisionDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pbddAllowedByPermissionsBoundary' - Specifies whether an action is allowed by a permissions boundary that is applied to an IAM entity (user or role). A value of @true@ means that the permissions boundary does not deny the action. This means that the policy includes an @Allow@ statement that matches the request. In this case, if an identity-based policy also allows the action, the request is allowed. A value of @false@ means that either the requested action is not allowed (implicitly denied) or that the action is explicitly denied by the permissions boundary. In both of these cases, the action is not allowed, regardless of the identity-based policy.
permissionsBoundaryDecisionDetail ::
  PermissionsBoundaryDecisionDetail
permissionsBoundaryDecisionDetail =
  PermissionsBoundaryDecisionDetail'
    { _pbddAllowedByPermissionsBoundary =
        Nothing
    }

-- | Specifies whether an action is allowed by a permissions boundary that is applied to an IAM entity (user or role). A value of @true@ means that the permissions boundary does not deny the action. This means that the policy includes an @Allow@ statement that matches the request. In this case, if an identity-based policy also allows the action, the request is allowed. A value of @false@ means that either the requested action is not allowed (implicitly denied) or that the action is explicitly denied by the permissions boundary. In both of these cases, the action is not allowed, regardless of the identity-based policy.
pbddAllowedByPermissionsBoundary :: Lens' PermissionsBoundaryDecisionDetail (Maybe Bool)
pbddAllowedByPermissionsBoundary = lens _pbddAllowedByPermissionsBoundary (\s a -> s {_pbddAllowedByPermissionsBoundary = a})

instance FromXML PermissionsBoundaryDecisionDetail where
  parseXML x =
    PermissionsBoundaryDecisionDetail'
      <$> (x .@? "AllowedByPermissionsBoundary")

instance Hashable PermissionsBoundaryDecisionDetail

instance NFData PermissionsBoundaryDecisionDetail

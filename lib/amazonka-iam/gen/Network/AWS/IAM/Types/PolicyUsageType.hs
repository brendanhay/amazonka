{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.PolicyUsageType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.PolicyUsageType where

import Network.AWS.Prelude

-- | The policy usage type that indicates whether the policy is used as a permissions policy or as the permissions boundary for an entity.
--
--
-- For more information about permissions boundaries, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_boundaries.html Permissions Boundaries for IAM Identities > in the /IAM User Guide/ .
data PolicyUsageType
  = PermissionsBoundary
  | PermissionsPolicy
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText PolicyUsageType where
  parser =
    takeLowerText >>= \case
      "permissionsboundary" -> pure PermissionsBoundary
      "permissionspolicy" -> pure PermissionsPolicy
      e ->
        fromTextError $
          "Failure parsing PolicyUsageType from value: '" <> e
            <> "'. Accepted values: permissionsboundary, permissionspolicy"

instance ToText PolicyUsageType where
  toText = \case
    PermissionsBoundary -> "PermissionsBoundary"
    PermissionsPolicy -> "PermissionsPolicy"

instance Hashable PolicyUsageType

instance NFData PolicyUsageType

instance ToByteString PolicyUsageType

instance ToQuery PolicyUsageType

instance ToHeader PolicyUsageType

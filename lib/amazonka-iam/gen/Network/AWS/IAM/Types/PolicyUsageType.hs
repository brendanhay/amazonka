{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.PolicyUsageType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.PolicyUsageType
  ( PolicyUsageType
      ( PolicyUsageType',
        PolicyUsageTypePermissionsPolicy,
        PolicyUsageTypePermissionsBoundary,
        fromPolicyUsageType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | The policy usage type that indicates whether the policy is used as a permissions policy or as the permissions boundary for an entity.
--
-- For more information about permissions boundaries, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_boundaries.html Permissions Boundaries for IAM Identities > in the /IAM User Guide/ .
newtype PolicyUsageType = PolicyUsageType'
  { fromPolicyUsageType ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern PolicyUsageTypePermissionsPolicy :: PolicyUsageType
pattern PolicyUsageTypePermissionsPolicy = PolicyUsageType' "PermissionsPolicy"

pattern PolicyUsageTypePermissionsBoundary :: PolicyUsageType
pattern PolicyUsageTypePermissionsBoundary = PolicyUsageType' "PermissionsBoundary"

{-# COMPLETE
  PolicyUsageTypePermissionsPolicy,
  PolicyUsageTypePermissionsBoundary,
  PolicyUsageType'
  #-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.PolicyUsageType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.PolicyUsageType
  ( PolicyUsageType
      ( ..,
        PolicyUsageType_PermissionsBoundary,
        PolicyUsageType_PermissionsPolicy
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | The policy usage type that indicates whether the policy is used as a
-- permissions policy or as the permissions boundary for an entity.
--
-- For more information about permissions boundaries, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_boundaries.html Permissions boundaries for IAM identities>
-- in the /IAM User Guide/.
newtype PolicyUsageType = PolicyUsageType'
  { fromPolicyUsageType ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern PolicyUsageType_PermissionsBoundary :: PolicyUsageType
pattern PolicyUsageType_PermissionsBoundary = PolicyUsageType' "PermissionsBoundary"

pattern PolicyUsageType_PermissionsPolicy :: PolicyUsageType
pattern PolicyUsageType_PermissionsPolicy = PolicyUsageType' "PermissionsPolicy"

{-# COMPLETE
  PolicyUsageType_PermissionsBoundary,
  PolicyUsageType_PermissionsPolicy,
  PolicyUsageType'
  #-}

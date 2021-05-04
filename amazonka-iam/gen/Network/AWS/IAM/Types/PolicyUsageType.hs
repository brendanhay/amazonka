{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Network.AWS.Prelude as Prelude

-- | The policy usage type that indicates whether the policy is used as a
-- permissions policy or as the permissions boundary for an entity.
--
-- For more information about permissions boundaries, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_boundaries.html Permissions boundaries for IAM identities>
-- in the /IAM User Guide/.
newtype PolicyUsageType = PolicyUsageType'
  { fromPolicyUsageType ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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

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
-- Module      : Amazonka.IAM.Types.PolicyUsageType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IAM.Types.PolicyUsageType
  ( PolicyUsageType
      ( ..,
        PolicyUsageType_PermissionsBoundary,
        PolicyUsageType_PermissionsPolicy
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The policy usage type that indicates whether the policy is used as a
-- permissions policy or as the permissions boundary for an entity.
--
-- For more information about permissions boundaries, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_boundaries.html Permissions boundaries for IAM identities>
-- in the /IAM User Guide/.
newtype PolicyUsageType = PolicyUsageType'
  { fromPolicyUsageType ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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

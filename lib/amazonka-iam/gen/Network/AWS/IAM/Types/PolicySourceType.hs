{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.PolicySourceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.PolicySourceType
  ( PolicySourceType
      ( PolicySourceType',
        PolicySourceTypeUser,
        PolicySourceTypeGroup,
        PolicySourceTypeRole,
        PolicySourceTypeAwsManaged,
        PolicySourceTypeUserManaged,
        PolicySourceTypeResource,
        PolicySourceTypeNone,
        fromPolicySourceType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype PolicySourceType = PolicySourceType'
  { fromPolicySourceType ::
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

pattern PolicySourceTypeUser :: PolicySourceType
pattern PolicySourceTypeUser = PolicySourceType' "user"

pattern PolicySourceTypeGroup :: PolicySourceType
pattern PolicySourceTypeGroup = PolicySourceType' "group"

pattern PolicySourceTypeRole :: PolicySourceType
pattern PolicySourceTypeRole = PolicySourceType' "role"

pattern PolicySourceTypeAwsManaged :: PolicySourceType
pattern PolicySourceTypeAwsManaged = PolicySourceType' "aws-managed"

pattern PolicySourceTypeUserManaged :: PolicySourceType
pattern PolicySourceTypeUserManaged = PolicySourceType' "user-managed"

pattern PolicySourceTypeResource :: PolicySourceType
pattern PolicySourceTypeResource = PolicySourceType' "resource"

pattern PolicySourceTypeNone :: PolicySourceType
pattern PolicySourceTypeNone = PolicySourceType' "none"

{-# COMPLETE
  PolicySourceTypeUser,
  PolicySourceTypeGroup,
  PolicySourceTypeRole,
  PolicySourceTypeAwsManaged,
  PolicySourceTypeUserManaged,
  PolicySourceTypeResource,
  PolicySourceTypeNone,
  PolicySourceType'
  #-}

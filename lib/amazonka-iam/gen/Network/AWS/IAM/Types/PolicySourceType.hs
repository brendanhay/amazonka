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
        User,
        Group,
        Role,
        AWSManaged,
        UserManaged,
        Resource,
        None
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype PolicySourceType = PolicySourceType' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern User :: PolicySourceType
pattern User = PolicySourceType' "user"

pattern Group :: PolicySourceType
pattern Group = PolicySourceType' "group"

pattern Role :: PolicySourceType
pattern Role = PolicySourceType' "role"

pattern AWSManaged :: PolicySourceType
pattern AWSManaged = PolicySourceType' "aws-managed"

pattern UserManaged :: PolicySourceType
pattern UserManaged = PolicySourceType' "user-managed"

pattern Resource :: PolicySourceType
pattern Resource = PolicySourceType' "resource"

pattern None :: PolicySourceType
pattern None = PolicySourceType' "none"

{-# COMPLETE
  User,
  Group,
  Role,
  AWSManaged,
  UserManaged,
  Resource,
  None,
  PolicySourceType'
  #-}

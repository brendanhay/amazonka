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
-- Module      : Network.AWS.IAM.Types.PolicySourceType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.PolicySourceType
  ( PolicySourceType
      ( ..,
        PolicySourceType_Aws_managed,
        PolicySourceType_Group,
        PolicySourceType_None,
        PolicySourceType_Resource,
        PolicySourceType_Role,
        PolicySourceType_User,
        PolicySourceType_User_managed
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype PolicySourceType = PolicySourceType'
  { fromPolicySourceType ::
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

pattern PolicySourceType_Aws_managed :: PolicySourceType
pattern PolicySourceType_Aws_managed = PolicySourceType' "aws-managed"

pattern PolicySourceType_Group :: PolicySourceType
pattern PolicySourceType_Group = PolicySourceType' "group"

pattern PolicySourceType_None :: PolicySourceType
pattern PolicySourceType_None = PolicySourceType' "none"

pattern PolicySourceType_Resource :: PolicySourceType
pattern PolicySourceType_Resource = PolicySourceType' "resource"

pattern PolicySourceType_Role :: PolicySourceType
pattern PolicySourceType_Role = PolicySourceType' "role"

pattern PolicySourceType_User :: PolicySourceType
pattern PolicySourceType_User = PolicySourceType' "user"

pattern PolicySourceType_User_managed :: PolicySourceType
pattern PolicySourceType_User_managed = PolicySourceType' "user-managed"

{-# COMPLETE
  PolicySourceType_Aws_managed,
  PolicySourceType_Group,
  PolicySourceType_None,
  PolicySourceType_Resource,
  PolicySourceType_Role,
  PolicySourceType_User,
  PolicySourceType_User_managed,
  PolicySourceType'
  #-}

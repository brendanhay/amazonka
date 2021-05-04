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
-- Module      : Network.AWS.IAM.Types.EntityType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.EntityType
  ( EntityType
      ( ..,
        EntityType_AWSManagedPolicy,
        EntityType_Group,
        EntityType_LocalManagedPolicy,
        EntityType_Role,
        EntityType_User
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype EntityType = EntityType'
  { fromEntityType ::
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

pattern EntityType_AWSManagedPolicy :: EntityType
pattern EntityType_AWSManagedPolicy = EntityType' "AWSManagedPolicy"

pattern EntityType_Group :: EntityType
pattern EntityType_Group = EntityType' "Group"

pattern EntityType_LocalManagedPolicy :: EntityType
pattern EntityType_LocalManagedPolicy = EntityType' "LocalManagedPolicy"

pattern EntityType_Role :: EntityType
pattern EntityType_Role = EntityType' "Role"

pattern EntityType_User :: EntityType
pattern EntityType_User = EntityType' "User"

{-# COMPLETE
  EntityType_AWSManagedPolicy,
  EntityType_Group,
  EntityType_LocalManagedPolicy,
  EntityType_Role,
  EntityType_User,
  EntityType'
  #-}

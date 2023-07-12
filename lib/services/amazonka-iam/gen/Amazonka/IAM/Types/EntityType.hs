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
-- Module      : Amazonka.IAM.Types.EntityType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IAM.Types.EntityType
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype EntityType = EntityType'
  { fromEntityType ::
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

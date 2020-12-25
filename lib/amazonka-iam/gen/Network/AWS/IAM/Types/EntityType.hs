{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.EntityType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.EntityType
  ( EntityType
      ( EntityType',
        EntityTypeUser,
        EntityTypeRole,
        EntityTypeGroup,
        EntityTypeLocalManagedPolicy,
        EntityTypeAWSManagedPolicy,
        fromEntityType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype EntityType = EntityType' {fromEntityType :: Core.Text}
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

pattern EntityTypeUser :: EntityType
pattern EntityTypeUser = EntityType' "User"

pattern EntityTypeRole :: EntityType
pattern EntityTypeRole = EntityType' "Role"

pattern EntityTypeGroup :: EntityType
pattern EntityTypeGroup = EntityType' "Group"

pattern EntityTypeLocalManagedPolicy :: EntityType
pattern EntityTypeLocalManagedPolicy = EntityType' "LocalManagedPolicy"

pattern EntityTypeAWSManagedPolicy :: EntityType
pattern EntityTypeAWSManagedPolicy = EntityType' "AWSManagedPolicy"

{-# COMPLETE
  EntityTypeUser,
  EntityTypeRole,
  EntityTypeGroup,
  EntityTypeLocalManagedPolicy,
  EntityTypeAWSManagedPolicy,
  EntityType'
  #-}

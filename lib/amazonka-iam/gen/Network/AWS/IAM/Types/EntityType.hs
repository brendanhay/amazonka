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
        ETAWSManagedPolicy,
        ETGroup,
        ETLocalManagedPolicy,
        ETRole,
        ETUser
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype EntityType = EntityType' Lude.Text
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

pattern ETAWSManagedPolicy :: EntityType
pattern ETAWSManagedPolicy = EntityType' "AWSManagedPolicy"

pattern ETGroup :: EntityType
pattern ETGroup = EntityType' "Group"

pattern ETLocalManagedPolicy :: EntityType
pattern ETLocalManagedPolicy = EntityType' "LocalManagedPolicy"

pattern ETRole :: EntityType
pattern ETRole = EntityType' "Role"

pattern ETUser :: EntityType
pattern ETUser = EntityType' "User"

{-# COMPLETE
  ETAWSManagedPolicy,
  ETGroup,
  ETLocalManagedPolicy,
  ETRole,
  ETUser,
  EntityType'
  #-}

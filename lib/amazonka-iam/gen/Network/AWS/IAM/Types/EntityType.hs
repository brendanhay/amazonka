{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.EntityType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.EntityType where

import Network.AWS.Prelude

data EntityType
  = ETAWSManagedPolicy
  | ETGroup
  | ETLocalManagedPolicy
  | ETRole
  | ETUser
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText EntityType where
  parser =
    takeLowerText >>= \case
      "awsmanagedpolicy" -> pure ETAWSManagedPolicy
      "group" -> pure ETGroup
      "localmanagedpolicy" -> pure ETLocalManagedPolicy
      "role" -> pure ETRole
      "user" -> pure ETUser
      e ->
        fromTextError $
          "Failure parsing EntityType from value: '" <> e
            <> "'. Accepted values: awsmanagedpolicy, group, localmanagedpolicy, role, user"

instance ToText EntityType where
  toText = \case
    ETAWSManagedPolicy -> "AWSManagedPolicy"
    ETGroup -> "Group"
    ETLocalManagedPolicy -> "LocalManagedPolicy"
    ETRole -> "Role"
    ETUser -> "User"

instance Hashable EntityType

instance NFData EntityType

instance ToByteString EntityType

instance ToQuery EntityType

instance ToHeader EntityType

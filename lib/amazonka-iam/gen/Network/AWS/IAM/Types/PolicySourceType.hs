{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.PolicySourceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.PolicySourceType where

import Network.AWS.Prelude

data PolicySourceType
  = AWSManaged
  | Group
  | None
  | Resource
  | Role
  | User
  | UserManaged
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

instance FromText PolicySourceType where
  parser =
    takeLowerText >>= \case
      "aws-managed" -> pure AWSManaged
      "group" -> pure Group
      "none" -> pure None
      "resource" -> pure Resource
      "role" -> pure Role
      "user" -> pure User
      "user-managed" -> pure UserManaged
      e ->
        fromTextError $
          "Failure parsing PolicySourceType from value: '" <> e
            <> "'. Accepted values: aws-managed, group, none, resource, role, user, user-managed"

instance ToText PolicySourceType where
  toText = \case
    AWSManaged -> "aws-managed"
    Group -> "group"
    None -> "none"
    Resource -> "resource"
    Role -> "role"
    User -> "user"
    UserManaged -> "user-managed"

instance Hashable PolicySourceType

instance NFData PolicySourceType

instance ToByteString PolicySourceType

instance ToQuery PolicySourceType

instance ToHeader PolicySourceType

instance FromXML PolicySourceType where
  parseXML = parseXMLText "PolicySourceType"

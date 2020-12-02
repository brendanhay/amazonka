{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.PolicyOwnerEntityType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.PolicyOwnerEntityType where

import Network.AWS.Prelude

data PolicyOwnerEntityType
  = POETGroup
  | POETRole
  | POETUser
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

instance FromText PolicyOwnerEntityType where
  parser =
    takeLowerText >>= \case
      "group" -> pure POETGroup
      "role" -> pure POETRole
      "user" -> pure POETUser
      e ->
        fromTextError $
          "Failure parsing PolicyOwnerEntityType from value: '" <> e
            <> "'. Accepted values: group, role, user"

instance ToText PolicyOwnerEntityType where
  toText = \case
    POETGroup -> "GROUP"
    POETRole -> "ROLE"
    POETUser -> "USER"

instance Hashable PolicyOwnerEntityType

instance NFData PolicyOwnerEntityType

instance ToByteString PolicyOwnerEntityType

instance ToQuery PolicyOwnerEntityType

instance ToHeader PolicyOwnerEntityType

instance FromXML PolicyOwnerEntityType where
  parseXML = parseXMLText "PolicyOwnerEntityType"

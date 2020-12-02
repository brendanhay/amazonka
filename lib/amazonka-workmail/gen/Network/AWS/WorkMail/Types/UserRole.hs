{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.Types.UserRole
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkMail.Types.UserRole where

import Network.AWS.Prelude

data UserRole
  = URResource
  | URSystemUser
  | URUser
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

instance FromText UserRole where
  parser =
    takeLowerText >>= \case
      "resource" -> pure URResource
      "system_user" -> pure URSystemUser
      "user" -> pure URUser
      e ->
        fromTextError $
          "Failure parsing UserRole from value: '" <> e
            <> "'. Accepted values: resource, system_user, user"

instance ToText UserRole where
  toText = \case
    URResource -> "RESOURCE"
    URSystemUser -> "SYSTEM_USER"
    URUser -> "USER"

instance Hashable UserRole

instance NFData UserRole

instance ToByteString UserRole

instance ToQuery UserRole

instance ToHeader UserRole

instance FromJSON UserRole where
  parseJSON = parseJSONText "UserRole"

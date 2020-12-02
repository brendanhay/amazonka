{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.UserType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.UserType where

import Network.AWS.Prelude

data UserType
  = UTAdmin
  | UTMinimaluser
  | UTPoweruser
  | UTUser
  | UTWorkspacesuser
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

instance FromText UserType where
  parser =
    takeLowerText >>= \case
      "admin" -> pure UTAdmin
      "minimaluser" -> pure UTMinimaluser
      "poweruser" -> pure UTPoweruser
      "user" -> pure UTUser
      "workspacesuser" -> pure UTWorkspacesuser
      e ->
        fromTextError $
          "Failure parsing UserType from value: '" <> e
            <> "'. Accepted values: admin, minimaluser, poweruser, user, workspacesuser"

instance ToText UserType where
  toText = \case
    UTAdmin -> "ADMIN"
    UTMinimaluser -> "MINIMALUSER"
    UTPoweruser -> "POWERUSER"
    UTUser -> "USER"
    UTWorkspacesuser -> "WORKSPACESUSER"

instance Hashable UserType

instance NFData UserType

instance ToByteString UserType

instance ToQuery UserType

instance ToHeader UserType

instance ToJSON UserType where
  toJSON = toJSONText

instance FromJSON UserType where
  parseJSON = parseJSONText "UserType"

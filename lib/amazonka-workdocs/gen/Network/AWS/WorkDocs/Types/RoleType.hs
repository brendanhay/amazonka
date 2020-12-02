{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.RoleType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.RoleType where

import Network.AWS.Prelude

data RoleType
  = Contributor
  | Coowner
  | Owner
  | Viewer
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

instance FromText RoleType where
  parser =
    takeLowerText >>= \case
      "contributor" -> pure Contributor
      "coowner" -> pure Coowner
      "owner" -> pure Owner
      "viewer" -> pure Viewer
      e ->
        fromTextError $
          "Failure parsing RoleType from value: '" <> e
            <> "'. Accepted values: contributor, coowner, owner, viewer"

instance ToText RoleType where
  toText = \case
    Contributor -> "CONTRIBUTOR"
    Coowner -> "COOWNER"
    Owner -> "OWNER"
    Viewer -> "VIEWER"

instance Hashable RoleType

instance NFData RoleType

instance ToByteString RoleType

instance ToQuery RoleType

instance ToHeader RoleType

instance ToJSON RoleType where
  toJSON = toJSONText

instance FromJSON RoleType where
  parseJSON = parseJSONText "RoleType"

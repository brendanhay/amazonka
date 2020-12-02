{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.RolePermissionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.RolePermissionType where

import Network.AWS.Prelude

data RolePermissionType
  = Direct
  | Inherited
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

instance FromText RolePermissionType where
  parser =
    takeLowerText >>= \case
      "direct" -> pure Direct
      "inherited" -> pure Inherited
      e ->
        fromTextError $
          "Failure parsing RolePermissionType from value: '" <> e
            <> "'. Accepted values: direct, inherited"

instance ToText RolePermissionType where
  toText = \case
    Direct -> "DIRECT"
    Inherited -> "INHERITED"

instance Hashable RolePermissionType

instance NFData RolePermissionType

instance ToByteString RolePermissionType

instance ToQuery RolePermissionType

instance ToHeader RolePermissionType

instance FromJSON RolePermissionType where
  parseJSON = parseJSONText "RolePermissionType"

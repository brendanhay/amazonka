{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.DocumentPermissionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.DocumentPermissionType where

import Network.AWS.Prelude

data DocumentPermissionType = Share
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

instance FromText DocumentPermissionType where
  parser =
    takeLowerText >>= \case
      "share" -> pure Share
      e ->
        fromTextError $
          "Failure parsing DocumentPermissionType from value: '" <> e
            <> "'. Accepted values: share"

instance ToText DocumentPermissionType where
  toText = \case
    Share -> "Share"

instance Hashable DocumentPermissionType

instance NFData DocumentPermissionType

instance ToByteString DocumentPermissionType

instance ToQuery DocumentPermissionType

instance ToHeader DocumentPermissionType

instance ToJSON DocumentPermissionType where
  toJSON = toJSONText

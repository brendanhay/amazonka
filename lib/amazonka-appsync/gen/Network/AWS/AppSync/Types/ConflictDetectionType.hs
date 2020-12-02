{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.ConflictDetectionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.ConflictDetectionType where

import Network.AWS.Prelude

data ConflictDetectionType
  = CDTNone
  | CDTVersion
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

instance FromText ConflictDetectionType where
  parser =
    takeLowerText >>= \case
      "none" -> pure CDTNone
      "version" -> pure CDTVersion
      e ->
        fromTextError $
          "Failure parsing ConflictDetectionType from value: '" <> e
            <> "'. Accepted values: none, version"

instance ToText ConflictDetectionType where
  toText = \case
    CDTNone -> "NONE"
    CDTVersion -> "VERSION"

instance Hashable ConflictDetectionType

instance NFData ConflictDetectionType

instance ToByteString ConflictDetectionType

instance ToQuery ConflictDetectionType

instance ToHeader ConflictDetectionType

instance ToJSON ConflictDetectionType where
  toJSON = toJSONText

instance FromJSON ConflictDetectionType where
  parseJSON = parseJSONText "ConflictDetectionType"

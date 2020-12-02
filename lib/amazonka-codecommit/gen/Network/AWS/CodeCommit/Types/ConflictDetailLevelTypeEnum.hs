{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.ConflictDetailLevelTypeEnum
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.ConflictDetailLevelTypeEnum where

import Network.AWS.Prelude

data ConflictDetailLevelTypeEnum
  = FileLevel
  | LineLevel
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

instance FromText ConflictDetailLevelTypeEnum where
  parser =
    takeLowerText >>= \case
      "file_level" -> pure FileLevel
      "line_level" -> pure LineLevel
      e ->
        fromTextError $
          "Failure parsing ConflictDetailLevelTypeEnum from value: '" <> e
            <> "'. Accepted values: file_level, line_level"

instance ToText ConflictDetailLevelTypeEnum where
  toText = \case
    FileLevel -> "FILE_LEVEL"
    LineLevel -> "LINE_LEVEL"

instance Hashable ConflictDetailLevelTypeEnum

instance NFData ConflictDetailLevelTypeEnum

instance ToByteString ConflictDetailLevelTypeEnum

instance ToQuery ConflictDetailLevelTypeEnum

instance ToHeader ConflictDetailLevelTypeEnum

instance ToJSON ConflictDetailLevelTypeEnum where
  toJSON = toJSONText

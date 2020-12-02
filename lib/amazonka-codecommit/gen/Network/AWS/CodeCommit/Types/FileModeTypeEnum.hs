{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.FileModeTypeEnum
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.FileModeTypeEnum where

import Network.AWS.Prelude

data FileModeTypeEnum
  = Executable
  | Normal
  | Symlink
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

instance FromText FileModeTypeEnum where
  parser =
    takeLowerText >>= \case
      "executable" -> pure Executable
      "normal" -> pure Normal
      "symlink" -> pure Symlink
      e ->
        fromTextError $
          "Failure parsing FileModeTypeEnum from value: '" <> e
            <> "'. Accepted values: executable, normal, symlink"

instance ToText FileModeTypeEnum where
  toText = \case
    Executable -> "EXECUTABLE"
    Normal -> "NORMAL"
    Symlink -> "SYMLINK"

instance Hashable FileModeTypeEnum

instance NFData FileModeTypeEnum

instance ToByteString FileModeTypeEnum

instance ToQuery FileModeTypeEnum

instance ToHeader FileModeTypeEnum

instance ToJSON FileModeTypeEnum where
  toJSON = toJSONText

instance FromJSON FileModeTypeEnum where
  parseJSON = parseJSONText "FileModeTypeEnum"

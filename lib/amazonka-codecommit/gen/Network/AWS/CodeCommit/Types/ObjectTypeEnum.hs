{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.ObjectTypeEnum
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.ObjectTypeEnum where

import Network.AWS.Prelude

data ObjectTypeEnum
  = Directory
  | File
  | GitLink
  | SymbolicLink
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

instance FromText ObjectTypeEnum where
  parser =
    takeLowerText >>= \case
      "directory" -> pure Directory
      "file" -> pure File
      "git_link" -> pure GitLink
      "symbolic_link" -> pure SymbolicLink
      e ->
        fromTextError $
          "Failure parsing ObjectTypeEnum from value: '" <> e
            <> "'. Accepted values: directory, file, git_link, symbolic_link"

instance ToText ObjectTypeEnum where
  toText = \case
    Directory -> "DIRECTORY"
    File -> "FILE"
    GitLink -> "GIT_LINK"
    SymbolicLink -> "SYMBOLIC_LINK"

instance Hashable ObjectTypeEnum

instance NFData ObjectTypeEnum

instance ToByteString ObjectTypeEnum

instance ToQuery ObjectTypeEnum

instance ToHeader ObjectTypeEnum

instance FromJSON ObjectTypeEnum where
  parseJSON = parseJSONText "ObjectTypeEnum"

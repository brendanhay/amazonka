{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.ReplacementTypeEnum
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.ReplacementTypeEnum where

import Network.AWS.Prelude

data ReplacementTypeEnum
  = KeepBase
  | KeepDestination
  | KeepSource
  | UseNewContent
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

instance FromText ReplacementTypeEnum where
  parser =
    takeLowerText >>= \case
      "keep_base" -> pure KeepBase
      "keep_destination" -> pure KeepDestination
      "keep_source" -> pure KeepSource
      "use_new_content" -> pure UseNewContent
      e ->
        fromTextError $
          "Failure parsing ReplacementTypeEnum from value: '" <> e
            <> "'. Accepted values: keep_base, keep_destination, keep_source, use_new_content"

instance ToText ReplacementTypeEnum where
  toText = \case
    KeepBase -> "KEEP_BASE"
    KeepDestination -> "KEEP_DESTINATION"
    KeepSource -> "KEEP_SOURCE"
    UseNewContent -> "USE_NEW_CONTENT"

instance Hashable ReplacementTypeEnum

instance NFData ReplacementTypeEnum

instance ToByteString ReplacementTypeEnum

instance ToQuery ReplacementTypeEnum

instance ToHeader ReplacementTypeEnum

instance ToJSON ReplacementTypeEnum where
  toJSON = toJSONText

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.DirectorySize
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.DirectorySize where

import Network.AWS.Prelude

data DirectorySize
  = Large
  | Small
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

instance FromText DirectorySize where
  parser =
    takeLowerText >>= \case
      "large" -> pure Large
      "small" -> pure Small
      e ->
        fromTextError $
          "Failure parsing DirectorySize from value: '" <> e
            <> "'. Accepted values: large, small"

instance ToText DirectorySize where
  toText = \case
    Large -> "Large"
    Small -> "Small"

instance Hashable DirectorySize

instance NFData DirectorySize

instance ToByteString DirectorySize

instance ToQuery DirectorySize

instance ToHeader DirectorySize

instance ToJSON DirectorySize where
  toJSON = toJSONText

instance FromJSON DirectorySize where
  parseJSON = parseJSONText "DirectorySize"

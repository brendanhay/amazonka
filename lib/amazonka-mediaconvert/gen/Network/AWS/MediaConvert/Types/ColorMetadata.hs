{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.ColorMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.ColorMetadata where

import Network.AWS.Prelude

-- | Choose Insert (INSERT) for this setting to include color metadata in this output. Choose Ignore (IGNORE) to exclude color metadata from this output. If you don't specify a value, the service sets this to Insert by default.
data ColorMetadata
  = Ignore
  | Insert
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

instance FromText ColorMetadata where
  parser =
    takeLowerText >>= \case
      "ignore" -> pure Ignore
      "insert" -> pure Insert
      e ->
        fromTextError $
          "Failure parsing ColorMetadata from value: '" <> e
            <> "'. Accepted values: ignore, insert"

instance ToText ColorMetadata where
  toText = \case
    Ignore -> "IGNORE"
    Insert -> "INSERT"

instance Hashable ColorMetadata

instance NFData ColorMetadata

instance ToByteString ColorMetadata

instance ToQuery ColorMetadata

instance ToHeader ColorMetadata

instance ToJSON ColorMetadata where
  toJSON = toJSONText

instance FromJSON ColorMetadata where
  parseJSON = parseJSONText "ColorMetadata"

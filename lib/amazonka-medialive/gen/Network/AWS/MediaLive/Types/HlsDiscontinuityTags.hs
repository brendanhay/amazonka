{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.HlsDiscontinuityTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.HlsDiscontinuityTags where

import Network.AWS.Prelude

-- | Hls Discontinuity Tags
data HlsDiscontinuityTags
  = HDTInsert
  | HDTNeverInsert
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

instance FromText HlsDiscontinuityTags where
  parser =
    takeLowerText >>= \case
      "insert" -> pure HDTInsert
      "never_insert" -> pure HDTNeverInsert
      e ->
        fromTextError $
          "Failure parsing HlsDiscontinuityTags from value: '" <> e
            <> "'. Accepted values: insert, never_insert"

instance ToText HlsDiscontinuityTags where
  toText = \case
    HDTInsert -> "INSERT"
    HDTNeverInsert -> "NEVER_INSERT"

instance Hashable HlsDiscontinuityTags

instance NFData HlsDiscontinuityTags

instance ToByteString HlsDiscontinuityTags

instance ToQuery HlsDiscontinuityTags

instance ToHeader HlsDiscontinuityTags

instance ToJSON HlsDiscontinuityTags where
  toJSON = toJSONText

instance FromJSON HlsDiscontinuityTags where
  parseJSON = parseJSONText "HlsDiscontinuityTags"

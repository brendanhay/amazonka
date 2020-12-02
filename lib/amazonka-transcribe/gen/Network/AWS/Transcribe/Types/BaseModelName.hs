{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.Types.BaseModelName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Transcribe.Types.BaseModelName where

import Network.AWS.Prelude

data BaseModelName
  = NarrowBand
  | WideBand
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

instance FromText BaseModelName where
  parser =
    takeLowerText >>= \case
      "narrowband" -> pure NarrowBand
      "wideband" -> pure WideBand
      e ->
        fromTextError $
          "Failure parsing BaseModelName from value: '" <> e
            <> "'. Accepted values: narrowband, wideband"

instance ToText BaseModelName where
  toText = \case
    NarrowBand -> "NarrowBand"
    WideBand -> "WideBand"

instance Hashable BaseModelName

instance NFData BaseModelName

instance ToByteString BaseModelName

instance ToQuery BaseModelName

instance ToHeader BaseModelName

instance ToJSON BaseModelName where
  toJSON = toJSONText

instance FromJSON BaseModelName where
  parseJSON = parseJSONText "BaseModelName"

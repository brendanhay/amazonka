{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.IFrameOnlyPlaylistType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.IFrameOnlyPlaylistType where

import Network.AWS.Prelude

-- | When set to "standard", an I-Frame only playlist will be written out for each video output in the output group. This I-Frame only playlist will contain byte range offsets pointing to the I-frame(s) in each segment.
data IFrameOnlyPlaylistType
  = IFOPTDisabled
  | IFOPTStandard
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

instance FromText IFrameOnlyPlaylistType where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure IFOPTDisabled
      "standard" -> pure IFOPTStandard
      e ->
        fromTextError $
          "Failure parsing IFrameOnlyPlaylistType from value: '" <> e
            <> "'. Accepted values: disabled, standard"

instance ToText IFrameOnlyPlaylistType where
  toText = \case
    IFOPTDisabled -> "DISABLED"
    IFOPTStandard -> "STANDARD"

instance Hashable IFrameOnlyPlaylistType

instance NFData IFrameOnlyPlaylistType

instance ToByteString IFrameOnlyPlaylistType

instance ToQuery IFrameOnlyPlaylistType

instance ToHeader IFrameOnlyPlaylistType

instance ToJSON IFrameOnlyPlaylistType where
  toJSON = toJSONText

instance FromJSON IFrameOnlyPlaylistType where
  parseJSON = parseJSONText "IFrameOnlyPlaylistType"

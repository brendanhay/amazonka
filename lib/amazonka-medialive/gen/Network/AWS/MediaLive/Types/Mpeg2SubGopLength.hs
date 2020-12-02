{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Mpeg2SubGopLength
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Mpeg2SubGopLength where

import Network.AWS.Prelude

-- | Mpeg2 Sub Gop Length
data Mpeg2SubGopLength
  = MSGLDynamic
  | MSGLFixed
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

instance FromText Mpeg2SubGopLength where
  parser =
    takeLowerText >>= \case
      "dynamic" -> pure MSGLDynamic
      "fixed" -> pure MSGLFixed
      e ->
        fromTextError $
          "Failure parsing Mpeg2SubGopLength from value: '" <> e
            <> "'. Accepted values: dynamic, fixed"

instance ToText Mpeg2SubGopLength where
  toText = \case
    MSGLDynamic -> "DYNAMIC"
    MSGLFixed -> "FIXED"

instance Hashable Mpeg2SubGopLength

instance NFData Mpeg2SubGopLength

instance ToByteString Mpeg2SubGopLength

instance ToQuery Mpeg2SubGopLength

instance ToHeader Mpeg2SubGopLength

instance ToJSON Mpeg2SubGopLength where
  toJSON = toJSONText

instance FromJSON Mpeg2SubGopLength where
  parseJSON = parseJSONText "Mpeg2SubGopLength"

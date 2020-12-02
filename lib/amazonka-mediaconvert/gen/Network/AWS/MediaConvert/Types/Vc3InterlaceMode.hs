{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Vc3InterlaceMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Vc3InterlaceMode where

import Network.AWS.Prelude

-- | Optional. Choose the scan line type for this output. If you don't specify a value, MediaConvert will create a progressive output.
data Vc3InterlaceMode
  = VIMInterlaced
  | VIMProgressive
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

instance FromText Vc3InterlaceMode where
  parser =
    takeLowerText >>= \case
      "interlaced" -> pure VIMInterlaced
      "progressive" -> pure VIMProgressive
      e ->
        fromTextError $
          "Failure parsing Vc3InterlaceMode from value: '" <> e
            <> "'. Accepted values: interlaced, progressive"

instance ToText Vc3InterlaceMode where
  toText = \case
    VIMInterlaced -> "INTERLACED"
    VIMProgressive -> "PROGRESSIVE"

instance Hashable Vc3InterlaceMode

instance NFData Vc3InterlaceMode

instance ToByteString Vc3InterlaceMode

instance ToQuery Vc3InterlaceMode

instance ToHeader Vc3InterlaceMode

instance ToJSON Vc3InterlaceMode where
  toJSON = toJSONText

instance FromJSON Vc3InterlaceMode where
  parseJSON = parseJSONText "Vc3InterlaceMode"

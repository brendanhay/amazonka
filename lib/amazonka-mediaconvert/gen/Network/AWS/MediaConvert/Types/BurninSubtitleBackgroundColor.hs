{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.BurninSubtitleBackgroundColor
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.BurninSubtitleBackgroundColor where

import Network.AWS.Prelude

-- | Specifies the color of the rectangle behind the captions.
--
-- All burn-in and DVB-Sub font settings must match.
data BurninSubtitleBackgroundColor
  = BSBCBlack
  | BSBCNone
  | BSBCWhite
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

instance FromText BurninSubtitleBackgroundColor where
  parser =
    takeLowerText >>= \case
      "black" -> pure BSBCBlack
      "none" -> pure BSBCNone
      "white" -> pure BSBCWhite
      e ->
        fromTextError $
          "Failure parsing BurninSubtitleBackgroundColor from value: '" <> e
            <> "'. Accepted values: black, none, white"

instance ToText BurninSubtitleBackgroundColor where
  toText = \case
    BSBCBlack -> "BLACK"
    BSBCNone -> "NONE"
    BSBCWhite -> "WHITE"

instance Hashable BurninSubtitleBackgroundColor

instance NFData BurninSubtitleBackgroundColor

instance ToByteString BurninSubtitleBackgroundColor

instance ToQuery BurninSubtitleBackgroundColor

instance ToHeader BurninSubtitleBackgroundColor

instance ToJSON BurninSubtitleBackgroundColor where
  toJSON = toJSONText

instance FromJSON BurninSubtitleBackgroundColor where
  parseJSON = parseJSONText "BurninSubtitleBackgroundColor"

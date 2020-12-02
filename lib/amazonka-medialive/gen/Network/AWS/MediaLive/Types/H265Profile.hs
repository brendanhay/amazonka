{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H265Profile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.H265Profile where

import Network.AWS.Prelude

-- | H265 Profile
data H265Profile
  = Main
  | Main10BIT
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

instance FromText H265Profile where
  parser =
    takeLowerText >>= \case
      "main" -> pure Main
      "main_10bit" -> pure Main10BIT
      e ->
        fromTextError $
          "Failure parsing H265Profile from value: '" <> e
            <> "'. Accepted values: main, main_10bit"

instance ToText H265Profile where
  toText = \case
    Main -> "MAIN"
    Main10BIT -> "MAIN_10BIT"

instance Hashable H265Profile

instance NFData H265Profile

instance ToByteString H265Profile

instance ToQuery H265Profile

instance ToHeader H265Profile

instance ToJSON H265Profile where
  toJSON = toJSONText

instance FromJSON H265Profile where
  parseJSON = parseJSONText "H265Profile"

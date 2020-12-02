{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.Types.Profile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaPackage.Types.Profile where

import Network.AWS.Prelude

data Profile
  = PHbbtv15
  | PNone
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

instance FromText Profile where
  parser =
    takeLowerText >>= \case
      "hbbtv_1_5" -> pure PHbbtv15
      "none" -> pure PNone
      e ->
        fromTextError $
          "Failure parsing Profile from value: '" <> e
            <> "'. Accepted values: hbbtv_1_5, none"

instance ToText Profile where
  toText = \case
    PHbbtv15 -> "HBBTV_1_5"
    PNone -> "NONE"

instance Hashable Profile

instance NFData Profile

instance ToByteString Profile

instance ToQuery Profile

instance ToHeader Profile

instance ToJSON Profile where
  toJSON = toJSONText

instance FromJSON Profile where
  parseJSON = parseJSONText "Profile"

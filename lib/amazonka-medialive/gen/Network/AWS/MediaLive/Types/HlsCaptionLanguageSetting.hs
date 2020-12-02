{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.HlsCaptionLanguageSetting
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.HlsCaptionLanguageSetting where

import Network.AWS.Prelude

-- | Hls Caption Language Setting
data HlsCaptionLanguageSetting
  = HCLSInsert
  | HCLSNone
  | HCLSOmit
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

instance FromText HlsCaptionLanguageSetting where
  parser =
    takeLowerText >>= \case
      "insert" -> pure HCLSInsert
      "none" -> pure HCLSNone
      "omit" -> pure HCLSOmit
      e ->
        fromTextError $
          "Failure parsing HlsCaptionLanguageSetting from value: '" <> e
            <> "'. Accepted values: insert, none, omit"

instance ToText HlsCaptionLanguageSetting where
  toText = \case
    HCLSInsert -> "INSERT"
    HCLSNone -> "NONE"
    HCLSOmit -> "OMIT"

instance Hashable HlsCaptionLanguageSetting

instance NFData HlsCaptionLanguageSetting

instance ToByteString HlsCaptionLanguageSetting

instance ToQuery HlsCaptionLanguageSetting

instance ToHeader HlsCaptionLanguageSetting

instance ToJSON HlsCaptionLanguageSetting where
  toJSON = toJSONText

instance FromJSON HlsCaptionLanguageSetting where
  parseJSON = parseJSONText "HlsCaptionLanguageSetting"

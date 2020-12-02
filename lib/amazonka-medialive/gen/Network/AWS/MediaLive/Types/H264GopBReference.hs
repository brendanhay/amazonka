{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H264GopBReference
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.H264GopBReference where

import Network.AWS.Prelude

-- | H264 Gop BReference
data H264GopBReference
  = HGBRDisabled
  | HGBREnabled
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

instance FromText H264GopBReference where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure HGBRDisabled
      "enabled" -> pure HGBREnabled
      e ->
        fromTextError $
          "Failure parsing H264GopBReference from value: '" <> e
            <> "'. Accepted values: disabled, enabled"

instance ToText H264GopBReference where
  toText = \case
    HGBRDisabled -> "DISABLED"
    HGBREnabled -> "ENABLED"

instance Hashable H264GopBReference

instance NFData H264GopBReference

instance ToByteString H264GopBReference

instance ToQuery H264GopBReference

instance ToHeader H264GopBReference

instance ToJSON H264GopBReference where
  toJSON = toJSONText

instance FromJSON H264GopBReference where
  parseJSON = parseJSONText "H264GopBReference"

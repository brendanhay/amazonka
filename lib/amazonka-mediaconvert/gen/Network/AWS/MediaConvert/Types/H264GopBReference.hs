{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H264GopBReference
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.H264GopBReference where

import Network.AWS.Prelude

-- | If enable, use reference B frames for GOP structures that have B frames > 1.
data H264GopBReference
  = HGBRGDisabled
  | HGBRGEnabled
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
      "disabled" -> pure HGBRGDisabled
      "enabled" -> pure HGBRGEnabled
      e ->
        fromTextError $
          "Failure parsing H264GopBReference from value: '" <> e
            <> "'. Accepted values: disabled, enabled"

instance ToText H264GopBReference where
  toText = \case
    HGBRGDisabled -> "DISABLED"
    HGBRGEnabled -> "ENABLED"

instance Hashable H264GopBReference

instance NFData H264GopBReference

instance ToByteString H264GopBReference

instance ToQuery H264GopBReference

instance ToHeader H264GopBReference

instance ToJSON H264GopBReference where
  toJSON = toJSONText

instance FromJSON H264GopBReference where
  parseJSON = parseJSONText "H264GopBReference"

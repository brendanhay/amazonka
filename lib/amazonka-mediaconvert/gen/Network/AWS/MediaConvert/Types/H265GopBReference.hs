{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H265GopBReference
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.H265GopBReference where

import Network.AWS.Prelude

-- | If enable, use reference B frames for GOP structures that have B frames > 1.
data H265GopBReference
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

instance FromText H265GopBReference where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure HGBRDisabled
      "enabled" -> pure HGBREnabled
      e ->
        fromTextError $
          "Failure parsing H265GopBReference from value: '" <> e
            <> "'. Accepted values: disabled, enabled"

instance ToText H265GopBReference where
  toText = \case
    HGBRDisabled -> "DISABLED"
    HGBREnabled -> "ENABLED"

instance Hashable H265GopBReference

instance NFData H265GopBReference

instance ToByteString H265GopBReference

instance ToQuery H265GopBReference

instance ToHeader H265GopBReference

instance ToJSON H265GopBReference where
  toJSON = toJSONText

instance FromJSON H265GopBReference where
  parseJSON = parseJSONText "H265GopBReference"

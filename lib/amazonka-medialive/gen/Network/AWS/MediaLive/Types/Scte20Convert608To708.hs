{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Scte20Convert608To708
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Scte20Convert608To708 where

import Network.AWS.Prelude

-- | Scte20 Convert608 To708
data Scte20Convert608To708
  = SCTDisabled
  | SCTUpconvert
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

instance FromText Scte20Convert608To708 where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure SCTDisabled
      "upconvert" -> pure SCTUpconvert
      e ->
        fromTextError $
          "Failure parsing Scte20Convert608To708 from value: '" <> e
            <> "'. Accepted values: disabled, upconvert"

instance ToText Scte20Convert608To708 where
  toText = \case
    SCTDisabled -> "DISABLED"
    SCTUpconvert -> "UPCONVERT"

instance Hashable Scte20Convert608To708

instance NFData Scte20Convert608To708

instance ToByteString Scte20Convert608To708

instance ToQuery Scte20Convert608To708

instance ToHeader Scte20Convert608To708

instance ToJSON Scte20Convert608To708 where
  toJSON = toJSONText

instance FromJSON Scte20Convert608To708 where
  parseJSON = parseJSONText "Scte20Convert608To708"

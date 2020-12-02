{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.EmbeddedConvert608To708
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.EmbeddedConvert608To708 where

import Network.AWS.Prelude

-- | Embedded Convert608 To708
data EmbeddedConvert608To708
  = ECTDisabled
  | ECTUpconvert
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

instance FromText EmbeddedConvert608To708 where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure ECTDisabled
      "upconvert" -> pure ECTUpconvert
      e ->
        fromTextError $
          "Failure parsing EmbeddedConvert608To708 from value: '" <> e
            <> "'. Accepted values: disabled, upconvert"

instance ToText EmbeddedConvert608To708 where
  toText = \case
    ECTDisabled -> "DISABLED"
    ECTUpconvert -> "UPCONVERT"

instance Hashable EmbeddedConvert608To708

instance NFData EmbeddedConvert608To708

instance ToByteString EmbeddedConvert608To708

instance ToQuery EmbeddedConvert608To708

instance ToHeader EmbeddedConvert608To708

instance ToJSON EmbeddedConvert608To708 where
  toJSON = toJSONText

instance FromJSON EmbeddedConvert608To708 where
  parseJSON = parseJSONText "EmbeddedConvert608To708"

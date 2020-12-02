{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.AacSpec
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.AacSpec where

import Network.AWS.Prelude

-- | Aac Spec
data AacSpec
  = ASMPEG2
  | ASMPEG4
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

instance FromText AacSpec where
  parser =
    takeLowerText >>= \case
      "mpeg2" -> pure ASMPEG2
      "mpeg4" -> pure ASMPEG4
      e ->
        fromTextError $
          "Failure parsing AacSpec from value: '" <> e
            <> "'. Accepted values: mpeg2, mpeg4"

instance ToText AacSpec where
  toText = \case
    ASMPEG2 -> "MPEG2"
    ASMPEG4 -> "MPEG4"

instance Hashable AacSpec

instance NFData AacSpec

instance ToByteString AacSpec

instance ToQuery AacSpec

instance ToHeader AacSpec

instance ToJSON AacSpec where
  toJSON = toJSONText

instance FromJSON AacSpec where
  parseJSON = parseJSONText "AacSpec"

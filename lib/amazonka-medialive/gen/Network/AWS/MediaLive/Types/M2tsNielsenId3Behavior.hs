{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.M2tsNielsenId3Behavior
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.M2tsNielsenId3Behavior where

import Network.AWS.Prelude

-- | M2ts Nielsen Id3 Behavior
data M2tsNielsenId3Behavior
  = MNIBNoPassthrough
  | MNIBPassthrough
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

instance FromText M2tsNielsenId3Behavior where
  parser =
    takeLowerText >>= \case
      "no_passthrough" -> pure MNIBNoPassthrough
      "passthrough" -> pure MNIBPassthrough
      e ->
        fromTextError $
          "Failure parsing M2tsNielsenId3Behavior from value: '" <> e
            <> "'. Accepted values: no_passthrough, passthrough"

instance ToText M2tsNielsenId3Behavior where
  toText = \case
    MNIBNoPassthrough -> "NO_PASSTHROUGH"
    MNIBPassthrough -> "PASSTHROUGH"

instance Hashable M2tsNielsenId3Behavior

instance NFData M2tsNielsenId3Behavior

instance ToByteString M2tsNielsenId3Behavior

instance ToQuery M2tsNielsenId3Behavior

instance ToHeader M2tsNielsenId3Behavior

instance ToJSON M2tsNielsenId3Behavior where
  toJSON = toJSONText

instance FromJSON M2tsNielsenId3Behavior where
  parseJSON = parseJSONText "M2tsNielsenId3Behavior"

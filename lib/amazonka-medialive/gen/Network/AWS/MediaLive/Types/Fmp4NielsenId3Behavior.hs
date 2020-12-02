{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Fmp4NielsenId3Behavior
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Fmp4NielsenId3Behavior where

import Network.AWS.Prelude

-- | Fmp4 Nielsen Id3 Behavior
data Fmp4NielsenId3Behavior
  = FNIBNoPassthrough
  | FNIBPassthrough
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

instance FromText Fmp4NielsenId3Behavior where
  parser =
    takeLowerText >>= \case
      "no_passthrough" -> pure FNIBNoPassthrough
      "passthrough" -> pure FNIBPassthrough
      e ->
        fromTextError $
          "Failure parsing Fmp4NielsenId3Behavior from value: '" <> e
            <> "'. Accepted values: no_passthrough, passthrough"

instance ToText Fmp4NielsenId3Behavior where
  toText = \case
    FNIBNoPassthrough -> "NO_PASSTHROUGH"
    FNIBPassthrough -> "PASSTHROUGH"

instance Hashable Fmp4NielsenId3Behavior

instance NFData Fmp4NielsenId3Behavior

instance ToByteString Fmp4NielsenId3Behavior

instance ToQuery Fmp4NielsenId3Behavior

instance ToHeader Fmp4NielsenId3Behavior

instance ToJSON Fmp4NielsenId3Behavior where
  toJSON = toJSONText

instance FromJSON Fmp4NielsenId3Behavior where
  parseJSON = parseJSONText "Fmp4NielsenId3Behavior"

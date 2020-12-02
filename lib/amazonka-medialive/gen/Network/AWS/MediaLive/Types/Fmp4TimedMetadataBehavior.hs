{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Fmp4TimedMetadataBehavior
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Fmp4TimedMetadataBehavior where

import Network.AWS.Prelude

-- | Fmp4 Timed Metadata Behavior
data Fmp4TimedMetadataBehavior
  = FTMBNoPassthrough
  | FTMBPassthrough
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

instance FromText Fmp4TimedMetadataBehavior where
  parser =
    takeLowerText >>= \case
      "no_passthrough" -> pure FTMBNoPassthrough
      "passthrough" -> pure FTMBPassthrough
      e ->
        fromTextError $
          "Failure parsing Fmp4TimedMetadataBehavior from value: '" <> e
            <> "'. Accepted values: no_passthrough, passthrough"

instance ToText Fmp4TimedMetadataBehavior where
  toText = \case
    FTMBNoPassthrough -> "NO_PASSTHROUGH"
    FTMBPassthrough -> "PASSTHROUGH"

instance Hashable Fmp4TimedMetadataBehavior

instance NFData Fmp4TimedMetadataBehavior

instance ToByteString Fmp4TimedMetadataBehavior

instance ToQuery Fmp4TimedMetadataBehavior

instance ToHeader Fmp4TimedMetadataBehavior

instance ToJSON Fmp4TimedMetadataBehavior where
  toJSON = toJSONText

instance FromJSON Fmp4TimedMetadataBehavior where
  parseJSON = parseJSONText "Fmp4TimedMetadataBehavior"

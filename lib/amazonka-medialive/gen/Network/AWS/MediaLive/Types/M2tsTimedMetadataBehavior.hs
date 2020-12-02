{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.M2tsTimedMetadataBehavior
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.M2tsTimedMetadataBehavior where

import Network.AWS.Prelude

-- | M2ts Timed Metadata Behavior
data M2tsTimedMetadataBehavior
  = MTMBNoPassthrough
  | MTMBPassthrough
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

instance FromText M2tsTimedMetadataBehavior where
  parser =
    takeLowerText >>= \case
      "no_passthrough" -> pure MTMBNoPassthrough
      "passthrough" -> pure MTMBPassthrough
      e ->
        fromTextError $
          "Failure parsing M2tsTimedMetadataBehavior from value: '" <> e
            <> "'. Accepted values: no_passthrough, passthrough"

instance ToText M2tsTimedMetadataBehavior where
  toText = \case
    MTMBNoPassthrough -> "NO_PASSTHROUGH"
    MTMBPassthrough -> "PASSTHROUGH"

instance Hashable M2tsTimedMetadataBehavior

instance NFData M2tsTimedMetadataBehavior

instance ToByteString M2tsTimedMetadataBehavior

instance ToQuery M2tsTimedMetadataBehavior

instance ToHeader M2tsTimedMetadataBehavior

instance ToJSON M2tsTimedMetadataBehavior where
  toJSON = toJSONText

instance FromJSON M2tsTimedMetadataBehavior where
  parseJSON = parseJSONText "M2tsTimedMetadataBehavior"

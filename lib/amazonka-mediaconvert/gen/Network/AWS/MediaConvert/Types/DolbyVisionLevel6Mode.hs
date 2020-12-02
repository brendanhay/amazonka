{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.DolbyVisionLevel6Mode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.DolbyVisionLevel6Mode where

import Network.AWS.Prelude

-- | Use Dolby Vision Mode to choose how the service will handle Dolby Vision MaxCLL and MaxFALL properies.
data DolbyVisionLevel6Mode
  = Passthrough
  | Recalculate
  | Specify
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

instance FromText DolbyVisionLevel6Mode where
  parser =
    takeLowerText >>= \case
      "passthrough" -> pure Passthrough
      "recalculate" -> pure Recalculate
      "specify" -> pure Specify
      e ->
        fromTextError $
          "Failure parsing DolbyVisionLevel6Mode from value: '" <> e
            <> "'. Accepted values: passthrough, recalculate, specify"

instance ToText DolbyVisionLevel6Mode where
  toText = \case
    Passthrough -> "PASSTHROUGH"
    Recalculate -> "RECALCULATE"
    Specify -> "SPECIFY"

instance Hashable DolbyVisionLevel6Mode

instance NFData DolbyVisionLevel6Mode

instance ToByteString DolbyVisionLevel6Mode

instance ToQuery DolbyVisionLevel6Mode

instance ToHeader DolbyVisionLevel6Mode

instance ToJSON DolbyVisionLevel6Mode where
  toJSON = toJSONText

instance FromJSON DolbyVisionLevel6Mode where
  parseJSON = parseJSONText "DolbyVisionLevel6Mode"

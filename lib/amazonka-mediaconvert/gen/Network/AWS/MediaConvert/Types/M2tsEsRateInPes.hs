{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.M2tsEsRateInPes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.M2tsEsRateInPes where

import Network.AWS.Prelude

-- | Controls whether to include the ES Rate field in the PES header.
data M2tsEsRateInPes
  = Exclude
  | Include
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

instance FromText M2tsEsRateInPes where
  parser =
    takeLowerText >>= \case
      "exclude" -> pure Exclude
      "include" -> pure Include
      e ->
        fromTextError $
          "Failure parsing M2tsEsRateInPes from value: '" <> e
            <> "'. Accepted values: exclude, include"

instance ToText M2tsEsRateInPes where
  toText = \case
    Exclude -> "EXCLUDE"
    Include -> "INCLUDE"

instance Hashable M2tsEsRateInPes

instance NFData M2tsEsRateInPes

instance ToByteString M2tsEsRateInPes

instance ToQuery M2tsEsRateInPes

instance ToHeader M2tsEsRateInPes

instance ToJSON M2tsEsRateInPes where
  toJSON = toJSONText

instance FromJSON M2tsEsRateInPes where
  parseJSON = parseJSONText "M2tsEsRateInPes"

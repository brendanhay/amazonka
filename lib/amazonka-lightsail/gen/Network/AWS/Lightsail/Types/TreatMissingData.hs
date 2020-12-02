{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.TreatMissingData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.TreatMissingData where

import Network.AWS.Prelude

data TreatMissingData
  = Breaching
  | Ignore
  | Missing
  | NotBreaching
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

instance FromText TreatMissingData where
  parser =
    takeLowerText >>= \case
      "breaching" -> pure Breaching
      "ignore" -> pure Ignore
      "missing" -> pure Missing
      "notbreaching" -> pure NotBreaching
      e ->
        fromTextError $
          "Failure parsing TreatMissingData from value: '" <> e
            <> "'. Accepted values: breaching, ignore, missing, notbreaching"

instance ToText TreatMissingData where
  toText = \case
    Breaching -> "breaching"
    Ignore -> "ignore"
    Missing -> "missing"
    NotBreaching -> "notBreaching"

instance Hashable TreatMissingData

instance NFData TreatMissingData

instance ToByteString TreatMissingData

instance ToQuery TreatMissingData

instance ToHeader TreatMissingData

instance ToJSON TreatMissingData where
  toJSON = toJSONText

instance FromJSON TreatMissingData where
  parseJSON = parseJSONText "TreatMissingData"

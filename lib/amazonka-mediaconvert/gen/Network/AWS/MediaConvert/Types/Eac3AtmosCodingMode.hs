{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Eac3AtmosCodingMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Eac3AtmosCodingMode where

import Network.AWS.Prelude

-- | The coding mode for Dolby Digital Plus JOC (Atmos) is always 9.1.6 (CODING_MODE_9_1_6).
data Eac3AtmosCodingMode = CodingMode916
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

instance FromText Eac3AtmosCodingMode where
  parser =
    takeLowerText >>= \case
      "coding_mode_9_1_6" -> pure CodingMode916
      e ->
        fromTextError $
          "Failure parsing Eac3AtmosCodingMode from value: '" <> e
            <> "'. Accepted values: coding_mode_9_1_6"

instance ToText Eac3AtmosCodingMode where
  toText = \case
    CodingMode916 -> "CODING_MODE_9_1_6"

instance Hashable Eac3AtmosCodingMode

instance NFData Eac3AtmosCodingMode

instance ToByteString Eac3AtmosCodingMode

instance ToQuery Eac3AtmosCodingMode

instance ToHeader Eac3AtmosCodingMode

instance ToJSON Eac3AtmosCodingMode where
  toJSON = toJSONText

instance FromJSON Eac3AtmosCodingMode where
  parseJSON = parseJSONText "Eac3AtmosCodingMode"

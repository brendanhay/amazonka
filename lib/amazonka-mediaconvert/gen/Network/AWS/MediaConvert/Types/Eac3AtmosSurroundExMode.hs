{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Eac3AtmosSurroundExMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Eac3AtmosSurroundExMode where

import Network.AWS.Prelude

-- | Specify whether your input audio has an additional center rear surround channel matrix encoded into your left and right surround channels.
data Eac3AtmosSurroundExMode
  = EASEMDisabled
  | EASEMEnabled
  | EASEMNotIndicated
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

instance FromText Eac3AtmosSurroundExMode where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure EASEMDisabled
      "enabled" -> pure EASEMEnabled
      "not_indicated" -> pure EASEMNotIndicated
      e ->
        fromTextError $
          "Failure parsing Eac3AtmosSurroundExMode from value: '" <> e
            <> "'. Accepted values: disabled, enabled, not_indicated"

instance ToText Eac3AtmosSurroundExMode where
  toText = \case
    EASEMDisabled -> "DISABLED"
    EASEMEnabled -> "ENABLED"
    EASEMNotIndicated -> "NOT_INDICATED"

instance Hashable Eac3AtmosSurroundExMode

instance NFData Eac3AtmosSurroundExMode

instance ToByteString Eac3AtmosSurroundExMode

instance ToQuery Eac3AtmosSurroundExMode

instance ToHeader Eac3AtmosSurroundExMode

instance ToJSON Eac3AtmosSurroundExMode where
  toJSON = toJSONText

instance FromJSON Eac3AtmosSurroundExMode where
  parseJSON = parseJSONText "Eac3AtmosSurroundExMode"

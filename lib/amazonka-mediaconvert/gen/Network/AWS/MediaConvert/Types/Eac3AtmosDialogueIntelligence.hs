{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Eac3AtmosDialogueIntelligence
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Eac3AtmosDialogueIntelligence where

import Network.AWS.Prelude

-- | Enable Dolby Dialogue Intelligence to adjust loudness based on dialogue analysis.
data Eac3AtmosDialogueIntelligence
  = EADIDisabled
  | EADIEnabled
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

instance FromText Eac3AtmosDialogueIntelligence where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure EADIDisabled
      "enabled" -> pure EADIEnabled
      e ->
        fromTextError $
          "Failure parsing Eac3AtmosDialogueIntelligence from value: '" <> e
            <> "'. Accepted values: disabled, enabled"

instance ToText Eac3AtmosDialogueIntelligence where
  toText = \case
    EADIDisabled -> "DISABLED"
    EADIEnabled -> "ENABLED"

instance Hashable Eac3AtmosDialogueIntelligence

instance NFData Eac3AtmosDialogueIntelligence

instance ToByteString Eac3AtmosDialogueIntelligence

instance ToQuery Eac3AtmosDialogueIntelligence

instance ToHeader Eac3AtmosDialogueIntelligence

instance ToJSON Eac3AtmosDialogueIntelligence where
  toJSON = toJSONText

instance FromJSON Eac3AtmosDialogueIntelligence where
  parseJSON = parseJSONText "Eac3AtmosDialogueIntelligence"

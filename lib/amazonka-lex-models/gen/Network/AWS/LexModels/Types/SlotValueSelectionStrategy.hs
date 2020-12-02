{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.SlotValueSelectionStrategy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.SlotValueSelectionStrategy where

import Network.AWS.Prelude

data SlotValueSelectionStrategy
  = OriginalValue
  | TopResolution
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

instance FromText SlotValueSelectionStrategy where
  parser =
    takeLowerText >>= \case
      "original_value" -> pure OriginalValue
      "top_resolution" -> pure TopResolution
      e ->
        fromTextError $
          "Failure parsing SlotValueSelectionStrategy from value: '" <> e
            <> "'. Accepted values: original_value, top_resolution"

instance ToText SlotValueSelectionStrategy where
  toText = \case
    OriginalValue -> "ORIGINAL_VALUE"
    TopResolution -> "TOP_RESOLUTION"

instance Hashable SlotValueSelectionStrategy

instance NFData SlotValueSelectionStrategy

instance ToByteString SlotValueSelectionStrategy

instance ToQuery SlotValueSelectionStrategy

instance ToHeader SlotValueSelectionStrategy

instance ToJSON SlotValueSelectionStrategy where
  toJSON = toJSONText

instance FromJSON SlotValueSelectionStrategy where
  parseJSON = parseJSONText "SlotValueSelectionStrategy"

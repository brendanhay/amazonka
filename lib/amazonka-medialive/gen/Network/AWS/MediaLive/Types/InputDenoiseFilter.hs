{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputDenoiseFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputDenoiseFilter where

import Network.AWS.Prelude

-- | Input Denoise Filter
data InputDenoiseFilter
  = IDisabled
  | IEnabled
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

instance FromText InputDenoiseFilter where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure IDisabled
      "enabled" -> pure IEnabled
      e ->
        fromTextError $
          "Failure parsing InputDenoiseFilter from value: '" <> e
            <> "'. Accepted values: disabled, enabled"

instance ToText InputDenoiseFilter where
  toText = \case
    IDisabled -> "DISABLED"
    IEnabled -> "ENABLED"

instance Hashable InputDenoiseFilter

instance NFData InputDenoiseFilter

instance ToByteString InputDenoiseFilter

instance ToQuery InputDenoiseFilter

instance ToHeader InputDenoiseFilter

instance ToJSON InputDenoiseFilter where
  toJSON = toJSONText

instance FromJSON InputDenoiseFilter where
  parseJSON = parseJSONText "InputDenoiseFilter"

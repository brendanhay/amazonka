{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Eac3PhaseControl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Eac3PhaseControl where

import Network.AWS.Prelude

-- | Eac3 Phase Control
data Eac3PhaseControl
  = NoShift
  | Shift90Degrees
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

instance FromText Eac3PhaseControl where
  parser =
    takeLowerText >>= \case
      "no_shift" -> pure NoShift
      "shift_90_degrees" -> pure Shift90Degrees
      e ->
        fromTextError $
          "Failure parsing Eac3PhaseControl from value: '" <> e
            <> "'. Accepted values: no_shift, shift_90_degrees"

instance ToText Eac3PhaseControl where
  toText = \case
    NoShift -> "NO_SHIFT"
    Shift90Degrees -> "SHIFT_90_DEGREES"

instance Hashable Eac3PhaseControl

instance NFData Eac3PhaseControl

instance ToByteString Eac3PhaseControl

instance ToQuery Eac3PhaseControl

instance ToHeader Eac3PhaseControl

instance ToJSON Eac3PhaseControl where
  toJSON = toJSONText

instance FromJSON Eac3PhaseControl where
  parseJSON = parseJSONText "Eac3PhaseControl"

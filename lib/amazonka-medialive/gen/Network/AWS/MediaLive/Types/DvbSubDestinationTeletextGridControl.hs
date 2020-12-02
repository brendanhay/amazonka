{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.DvbSubDestinationTeletextGridControl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.DvbSubDestinationTeletextGridControl where

import Network.AWS.Prelude

-- | Dvb Sub Destination Teletext Grid Control
data DvbSubDestinationTeletextGridControl
  = DSDTGCFixed
  | DSDTGCScaled
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

instance FromText DvbSubDestinationTeletextGridControl where
  parser =
    takeLowerText >>= \case
      "fixed" -> pure DSDTGCFixed
      "scaled" -> pure DSDTGCScaled
      e ->
        fromTextError $
          "Failure parsing DvbSubDestinationTeletextGridControl from value: '" <> e
            <> "'. Accepted values: fixed, scaled"

instance ToText DvbSubDestinationTeletextGridControl where
  toText = \case
    DSDTGCFixed -> "FIXED"
    DSDTGCScaled -> "SCALED"

instance Hashable DvbSubDestinationTeletextGridControl

instance NFData DvbSubDestinationTeletextGridControl

instance ToByteString DvbSubDestinationTeletextGridControl

instance ToQuery DvbSubDestinationTeletextGridControl

instance ToHeader DvbSubDestinationTeletextGridControl

instance ToJSON DvbSubDestinationTeletextGridControl where
  toJSON = toJSONText

instance FromJSON DvbSubDestinationTeletextGridControl where
  parseJSON = parseJSONText "DvbSubDestinationTeletextGridControl"

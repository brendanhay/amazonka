{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.DashIsoMpdProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.DashIsoMpdProfile where

import Network.AWS.Prelude

-- | Specify whether your DASH profile is on-demand or main. When you choose Main profile (MAIN_PROFILE), the service signals  urn:mpeg:dash:profile:isoff-main:2011 in your .mpd DASH manifest. When you choose On-demand (ON_DEMAND_PROFILE), the service signals urn:mpeg:dash:profile:isoff-on-demand:2011 in your .mpd. When you choose On-demand, you must also set the output group setting Segment control (SegmentControl) to Single file (SINGLE_FILE).
data DashIsoMpdProfile
  = DIMPMainProfile
  | DIMPOnDemandProfile
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

instance FromText DashIsoMpdProfile where
  parser =
    takeLowerText >>= \case
      "main_profile" -> pure DIMPMainProfile
      "on_demand_profile" -> pure DIMPOnDemandProfile
      e ->
        fromTextError $
          "Failure parsing DashIsoMpdProfile from value: '" <> e
            <> "'. Accepted values: main_profile, on_demand_profile"

instance ToText DashIsoMpdProfile where
  toText = \case
    DIMPMainProfile -> "MAIN_PROFILE"
    DIMPOnDemandProfile -> "ON_DEMAND_PROFILE"

instance Hashable DashIsoMpdProfile

instance NFData DashIsoMpdProfile

instance ToByteString DashIsoMpdProfile

instance ToQuery DashIsoMpdProfile

instance ToHeader DashIsoMpdProfile

instance ToJSON DashIsoMpdProfile where
  toJSON = toJSONText

instance FromJSON DashIsoMpdProfile where
  parseJSON = parseJSONText "DashIsoMpdProfile"

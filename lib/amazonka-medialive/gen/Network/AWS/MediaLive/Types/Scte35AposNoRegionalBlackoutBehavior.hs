{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Scte35AposNoRegionalBlackoutBehavior
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Scte35AposNoRegionalBlackoutBehavior where

import Network.AWS.Prelude

-- | Scte35 Apos No Regional Blackout Behavior
data Scte35AposNoRegionalBlackoutBehavior
  = SANRBBFollow
  | SANRBBIgnore
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

instance FromText Scte35AposNoRegionalBlackoutBehavior where
  parser =
    takeLowerText >>= \case
      "follow" -> pure SANRBBFollow
      "ignore" -> pure SANRBBIgnore
      e ->
        fromTextError $
          "Failure parsing Scte35AposNoRegionalBlackoutBehavior from value: '" <> e
            <> "'. Accepted values: follow, ignore"

instance ToText Scte35AposNoRegionalBlackoutBehavior where
  toText = \case
    SANRBBFollow -> "FOLLOW"
    SANRBBIgnore -> "IGNORE"

instance Hashable Scte35AposNoRegionalBlackoutBehavior

instance NFData Scte35AposNoRegionalBlackoutBehavior

instance ToByteString Scte35AposNoRegionalBlackoutBehavior

instance ToQuery Scte35AposNoRegionalBlackoutBehavior

instance ToHeader Scte35AposNoRegionalBlackoutBehavior

instance ToJSON Scte35AposNoRegionalBlackoutBehavior where
  toJSON = toJSONText

instance FromJSON Scte35AposNoRegionalBlackoutBehavior where
  parseJSON = parseJSONText "Scte35AposNoRegionalBlackoutBehavior"

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Scte35SpliceInsertNoRegionalBlackoutBehavior
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Scte35SpliceInsertNoRegionalBlackoutBehavior where

import Network.AWS.Prelude

-- | Scte35 Splice Insert No Regional Blackout Behavior
data Scte35SpliceInsertNoRegionalBlackoutBehavior
  = SSINRBBFollow
  | SSINRBBIgnore
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

instance FromText Scte35SpliceInsertNoRegionalBlackoutBehavior where
  parser =
    takeLowerText >>= \case
      "follow" -> pure SSINRBBFollow
      "ignore" -> pure SSINRBBIgnore
      e ->
        fromTextError $
          "Failure parsing Scte35SpliceInsertNoRegionalBlackoutBehavior from value: '" <> e
            <> "'. Accepted values: follow, ignore"

instance ToText Scte35SpliceInsertNoRegionalBlackoutBehavior where
  toText = \case
    SSINRBBFollow -> "FOLLOW"
    SSINRBBIgnore -> "IGNORE"

instance Hashable Scte35SpliceInsertNoRegionalBlackoutBehavior

instance NFData Scte35SpliceInsertNoRegionalBlackoutBehavior

instance ToByteString Scte35SpliceInsertNoRegionalBlackoutBehavior

instance ToQuery Scte35SpliceInsertNoRegionalBlackoutBehavior

instance ToHeader Scte35SpliceInsertNoRegionalBlackoutBehavior

instance ToJSON Scte35SpliceInsertNoRegionalBlackoutBehavior where
  toJSON = toJSONText

instance FromJSON Scte35SpliceInsertNoRegionalBlackoutBehavior where
  parseJSON = parseJSONText "Scte35SpliceInsertNoRegionalBlackoutBehavior"

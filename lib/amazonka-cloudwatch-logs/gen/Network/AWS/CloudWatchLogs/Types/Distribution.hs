{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.Types.Distribution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchLogs.Types.Distribution where

import Network.AWS.Prelude

-- | The method used to distribute log data to the destination, which can be either random or grouped by log stream.
data Distribution
  = ByLogStream
  | Random
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

instance FromText Distribution where
  parser =
    takeLowerText >>= \case
      "bylogstream" -> pure ByLogStream
      "random" -> pure Random
      e ->
        fromTextError $
          "Failure parsing Distribution from value: '" <> e
            <> "'. Accepted values: bylogstream, random"

instance ToText Distribution where
  toText = \case
    ByLogStream -> "ByLogStream"
    Random -> "Random"

instance Hashable Distribution

instance NFData Distribution

instance ToByteString Distribution

instance ToQuery Distribution

instance ToHeader Distribution

instance ToJSON Distribution where
  toJSON = toJSONText

instance FromJSON Distribution where
  parseJSON = parseJSONText "Distribution"

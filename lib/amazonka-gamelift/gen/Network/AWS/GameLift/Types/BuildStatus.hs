{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.BuildStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.BuildStatus where

import Network.AWS.Prelude

data BuildStatus
  = Failed
  | Initialized
  | Ready
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

instance FromText BuildStatus where
  parser =
    takeLowerText >>= \case
      "failed" -> pure Failed
      "initialized" -> pure Initialized
      "ready" -> pure Ready
      e ->
        fromTextError $
          "Failure parsing BuildStatus from value: '" <> e
            <> "'. Accepted values: failed, initialized, ready"

instance ToText BuildStatus where
  toText = \case
    Failed -> "FAILED"
    Initialized -> "INITIALIZED"
    Ready -> "READY"

instance Hashable BuildStatus

instance NFData BuildStatus

instance ToByteString BuildStatus

instance ToQuery BuildStatus

instance ToHeader BuildStatus

instance ToJSON BuildStatus where
  toJSON = toJSONText

instance FromJSON BuildStatus where
  parseJSON = parseJSONText "BuildStatus"

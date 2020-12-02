{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.LexStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.LexStatus where

import Network.AWS.Prelude

data LexStatus
  = LSBuilding
  | LSFailed
  | LSNotBuilt
  | LSReady
  | LSReadyBasicTesting
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

instance FromText LexStatus where
  parser =
    takeLowerText >>= \case
      "building" -> pure LSBuilding
      "failed" -> pure LSFailed
      "not_built" -> pure LSNotBuilt
      "ready" -> pure LSReady
      "ready_basic_testing" -> pure LSReadyBasicTesting
      e ->
        fromTextError $
          "Failure parsing LexStatus from value: '" <> e
            <> "'. Accepted values: building, failed, not_built, ready, ready_basic_testing"

instance ToText LexStatus where
  toText = \case
    LSBuilding -> "BUILDING"
    LSFailed -> "FAILED"
    LSNotBuilt -> "NOT_BUILT"
    LSReady -> "READY"
    LSReadyBasicTesting -> "READY_BASIC_TESTING"

instance Hashable LexStatus

instance NFData LexStatus

instance ToByteString LexStatus

instance ToQuery LexStatus

instance ToHeader LexStatus

instance FromJSON LexStatus where
  parseJSON = parseJSONText "LexStatus"

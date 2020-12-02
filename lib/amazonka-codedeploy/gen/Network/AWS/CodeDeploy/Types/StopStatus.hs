{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.StopStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.StopStatus where

import Network.AWS.Prelude

data StopStatus
  = SSPending
  | SSSucceeded
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

instance FromText StopStatus where
  parser =
    takeLowerText >>= \case
      "pending" -> pure SSPending
      "succeeded" -> pure SSSucceeded
      e ->
        fromTextError $
          "Failure parsing StopStatus from value: '" <> e
            <> "'. Accepted values: pending, succeeded"

instance ToText StopStatus where
  toText = \case
    SSPending -> "Pending"
    SSSucceeded -> "Succeeded"

instance Hashable StopStatus

instance NFData StopStatus

instance ToByteString StopStatus

instance ToQuery StopStatus

instance ToHeader StopStatus

instance FromJSON StopStatus where
  parseJSON = parseJSONText "StopStatus"

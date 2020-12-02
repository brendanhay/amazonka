{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.RecorderStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.RecorderStatus where

import Network.AWS.Prelude

data RecorderStatus
  = RSFailure
  | RSPending
  | RSSuccess
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

instance FromText RecorderStatus where
  parser =
    takeLowerText >>= \case
      "failure" -> pure RSFailure
      "pending" -> pure RSPending
      "success" -> pure RSSuccess
      e ->
        fromTextError $
          "Failure parsing RecorderStatus from value: '" <> e
            <> "'. Accepted values: failure, pending, success"

instance ToText RecorderStatus where
  toText = \case
    RSFailure -> "Failure"
    RSPending -> "Pending"
    RSSuccess -> "Success"

instance Hashable RecorderStatus

instance NFData RecorderStatus

instance ToByteString RecorderStatus

instance ToQuery RecorderStatus

instance ToHeader RecorderStatus

instance FromJSON RecorderStatus where
  parseJSON = parseJSONText "RecorderStatus"

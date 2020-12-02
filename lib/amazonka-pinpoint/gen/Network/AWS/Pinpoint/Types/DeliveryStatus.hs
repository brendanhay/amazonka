{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.DeliveryStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.DeliveryStatus where

import Network.AWS.Prelude

data DeliveryStatus
  = Duplicate
  | OptOut
  | PermanentFailure
  | Successful
  | TemporaryFailure
  | Throttled
  | UnknownFailure
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

instance FromText DeliveryStatus where
  parser =
    takeLowerText >>= \case
      "duplicate" -> pure Duplicate
      "opt_out" -> pure OptOut
      "permanent_failure" -> pure PermanentFailure
      "successful" -> pure Successful
      "temporary_failure" -> pure TemporaryFailure
      "throttled" -> pure Throttled
      "unknown_failure" -> pure UnknownFailure
      e ->
        fromTextError $
          "Failure parsing DeliveryStatus from value: '" <> e
            <> "'. Accepted values: duplicate, opt_out, permanent_failure, successful, temporary_failure, throttled, unknown_failure"

instance ToText DeliveryStatus where
  toText = \case
    Duplicate -> "DUPLICATE"
    OptOut -> "OPT_OUT"
    PermanentFailure -> "PERMANENT_FAILURE"
    Successful -> "SUCCESSFUL"
    TemporaryFailure -> "TEMPORARY_FAILURE"
    Throttled -> "THROTTLED"
    UnknownFailure -> "UNKNOWN_FAILURE"

instance Hashable DeliveryStatus

instance NFData DeliveryStatus

instance ToByteString DeliveryStatus

instance ToQuery DeliveryStatus

instance ToHeader DeliveryStatus

instance FromJSON DeliveryStatus where
  parseJSON = parseJSONText "DeliveryStatus"

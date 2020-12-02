{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.DeliveryStreamStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.DeliveryStreamStatus where

import Network.AWS.Prelude

data DeliveryStreamStatus
  = Active
  | Creating
  | CreatingFailed
  | Deleting
  | DeletingFailed
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

instance FromText DeliveryStreamStatus where
  parser =
    takeLowerText >>= \case
      "active" -> pure Active
      "creating" -> pure Creating
      "creating_failed" -> pure CreatingFailed
      "deleting" -> pure Deleting
      "deleting_failed" -> pure DeletingFailed
      e ->
        fromTextError $
          "Failure parsing DeliveryStreamStatus from value: '" <> e
            <> "'. Accepted values: active, creating, creating_failed, deleting, deleting_failed"

instance ToText DeliveryStreamStatus where
  toText = \case
    Active -> "ACTIVE"
    Creating -> "CREATING"
    CreatingFailed -> "CREATING_FAILED"
    Deleting -> "DELETING"
    DeletingFailed -> "DELETING_FAILED"

instance Hashable DeliveryStreamStatus

instance NFData DeliveryStreamStatus

instance ToByteString DeliveryStreamStatus

instance ToQuery DeliveryStreamStatus

instance ToHeader DeliveryStreamStatus

instance FromJSON DeliveryStreamStatus where
  parseJSON = parseJSONText "DeliveryStreamStatus"

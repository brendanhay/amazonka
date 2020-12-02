{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.PublishingStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.PublishingStatus where

import Network.AWS.Prelude

data PublishingStatus
  = PendingVerification
  | Publishing
  | Stopped
  | UnableToPublishFixDestinationProperty
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

instance FromText PublishingStatus where
  parser =
    takeLowerText >>= \case
      "pending_verification" -> pure PendingVerification
      "publishing" -> pure Publishing
      "stopped" -> pure Stopped
      "unable_to_publish_fix_destination_property" -> pure UnableToPublishFixDestinationProperty
      e ->
        fromTextError $
          "Failure parsing PublishingStatus from value: '" <> e
            <> "'. Accepted values: pending_verification, publishing, stopped, unable_to_publish_fix_destination_property"

instance ToText PublishingStatus where
  toText = \case
    PendingVerification -> "PENDING_VERIFICATION"
    Publishing -> "PUBLISHING"
    Stopped -> "STOPPED"
    UnableToPublishFixDestinationProperty -> "UNABLE_TO_PUBLISH_FIX_DESTINATION_PROPERTY"

instance Hashable PublishingStatus

instance NFData PublishingStatus

instance ToByteString PublishingStatus

instance ToQuery PublishingStatus

instance ToHeader PublishingStatus

instance FromJSON PublishingStatus where
  parseJSON = parseJSONText "PublishingStatus"

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.EndpointStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.EndpointStatus where

import Network.AWS.Prelude

data EndpointStatus
  = ESCreating
  | ESDeleting
  | ESFailed
  | ESInService
  | ESUpdating
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

instance FromText EndpointStatus where
  parser =
    takeLowerText >>= \case
      "creating" -> pure ESCreating
      "deleting" -> pure ESDeleting
      "failed" -> pure ESFailed
      "in_service" -> pure ESInService
      "updating" -> pure ESUpdating
      e ->
        fromTextError $
          "Failure parsing EndpointStatus from value: '" <> e
            <> "'. Accepted values: creating, deleting, failed, in_service, updating"

instance ToText EndpointStatus where
  toText = \case
    ESCreating -> "CREATING"
    ESDeleting -> "DELETING"
    ESFailed -> "FAILED"
    ESInService -> "IN_SERVICE"
    ESUpdating -> "UPDATING"

instance Hashable EndpointStatus

instance NFData EndpointStatus

instance ToByteString EndpointStatus

instance ToQuery EndpointStatus

instance ToHeader EndpointStatus

instance ToJSON EndpointStatus where
  toJSON = toJSONText

instance FromJSON EndpointStatus where
  parseJSON = parseJSONText "EndpointStatus"

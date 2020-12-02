{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.EnrollmentStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.EnrollmentStatus where

import Network.AWS.Prelude

data EnrollmentStatus
  = ESDeregistering
  | ESDisassociating
  | ESInitialized
  | ESPending
  | ESRegistered
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

instance FromText EnrollmentStatus where
  parser =
    takeLowerText >>= \case
      "deregistering" -> pure ESDeregistering
      "disassociating" -> pure ESDisassociating
      "initialized" -> pure ESInitialized
      "pending" -> pure ESPending
      "registered" -> pure ESRegistered
      e ->
        fromTextError $
          "Failure parsing EnrollmentStatus from value: '" <> e
            <> "'. Accepted values: deregistering, disassociating, initialized, pending, registered"

instance ToText EnrollmentStatus where
  toText = \case
    ESDeregistering -> "DEREGISTERING"
    ESDisassociating -> "DISASSOCIATING"
    ESInitialized -> "INITIALIZED"
    ESPending -> "PENDING"
    ESRegistered -> "REGISTERED"

instance Hashable EnrollmentStatus

instance NFData EnrollmentStatus

instance ToByteString EnrollmentStatus

instance ToQuery EnrollmentStatus

instance ToHeader EnrollmentStatus

instance FromJSON EnrollmentStatus where
  parseJSON = parseJSONText "EnrollmentStatus"

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.StackInstanceDetailedStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.StackInstanceDetailedStatus where

import Network.AWS.Prelude

data StackInstanceDetailedStatus
  = SIDSCancelled
  | SIDSFailed
  | SIDSInoperable
  | SIDSPending
  | SIDSRunning
  | SIDSSucceeded
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

instance FromText StackInstanceDetailedStatus where
  parser =
    takeLowerText >>= \case
      "cancelled" -> pure SIDSCancelled
      "failed" -> pure SIDSFailed
      "inoperable" -> pure SIDSInoperable
      "pending" -> pure SIDSPending
      "running" -> pure SIDSRunning
      "succeeded" -> pure SIDSSucceeded
      e ->
        fromTextError $
          "Failure parsing StackInstanceDetailedStatus from value: '" <> e
            <> "'. Accepted values: cancelled, failed, inoperable, pending, running, succeeded"

instance ToText StackInstanceDetailedStatus where
  toText = \case
    SIDSCancelled -> "CANCELLED"
    SIDSFailed -> "FAILED"
    SIDSInoperable -> "INOPERABLE"
    SIDSPending -> "PENDING"
    SIDSRunning -> "RUNNING"
    SIDSSucceeded -> "SUCCEEDED"

instance Hashable StackInstanceDetailedStatus

instance NFData StackInstanceDetailedStatus

instance ToByteString StackInstanceDetailedStatus

instance ToQuery StackInstanceDetailedStatus

instance ToHeader StackInstanceDetailedStatus

instance FromXML StackInstanceDetailedStatus where
  parseXML = parseXMLText "StackInstanceDetailedStatus"

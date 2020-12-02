{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.InstanceRefreshStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.InstanceRefreshStatus where

import Network.AWS.Prelude

data InstanceRefreshStatus
  = IRSCancelled
  | IRSCancelling
  | IRSFailed
  | IRSInProgress
  | IRSPending
  | IRSSuccessful
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

instance FromText InstanceRefreshStatus where
  parser =
    takeLowerText >>= \case
      "cancelled" -> pure IRSCancelled
      "cancelling" -> pure IRSCancelling
      "failed" -> pure IRSFailed
      "inprogress" -> pure IRSInProgress
      "pending" -> pure IRSPending
      "successful" -> pure IRSSuccessful
      e ->
        fromTextError $
          "Failure parsing InstanceRefreshStatus from value: '" <> e
            <> "'. Accepted values: cancelled, cancelling, failed, inprogress, pending, successful"

instance ToText InstanceRefreshStatus where
  toText = \case
    IRSCancelled -> "Cancelled"
    IRSCancelling -> "Cancelling"
    IRSFailed -> "Failed"
    IRSInProgress -> "InProgress"
    IRSPending -> "Pending"
    IRSSuccessful -> "Successful"

instance Hashable InstanceRefreshStatus

instance NFData InstanceRefreshStatus

instance ToByteString InstanceRefreshStatus

instance ToQuery InstanceRefreshStatus

instance ToHeader InstanceRefreshStatus

instance FromXML InstanceRefreshStatus where
  parseXML = parseXMLText "InstanceRefreshStatus"

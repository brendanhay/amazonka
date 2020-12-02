{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.BundleTaskState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.BundleTaskState where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data BundleTaskState
  = BTSBundling
  | BTSCancelling
  | BTSComplete
  | BTSFailed
  | BTSPending
  | BTSStoring
  | BTSWaitingForShutdown
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

instance FromText BundleTaskState where
  parser =
    takeLowerText >>= \case
      "bundling" -> pure BTSBundling
      "cancelling" -> pure BTSCancelling
      "complete" -> pure BTSComplete
      "failed" -> pure BTSFailed
      "pending" -> pure BTSPending
      "storing" -> pure BTSStoring
      "waiting-for-shutdown" -> pure BTSWaitingForShutdown
      e ->
        fromTextError $
          "Failure parsing BundleTaskState from value: '" <> e
            <> "'. Accepted values: bundling, cancelling, complete, failed, pending, storing, waiting-for-shutdown"

instance ToText BundleTaskState where
  toText = \case
    BTSBundling -> "bundling"
    BTSCancelling -> "cancelling"
    BTSComplete -> "complete"
    BTSFailed -> "failed"
    BTSPending -> "pending"
    BTSStoring -> "storing"
    BTSWaitingForShutdown -> "waiting-for-shutdown"

instance Hashable BundleTaskState

instance NFData BundleTaskState

instance ToByteString BundleTaskState

instance ToQuery BundleTaskState

instance ToHeader BundleTaskState

instance FromXML BundleTaskState where
  parseXML = parseXMLText "BundleTaskState"

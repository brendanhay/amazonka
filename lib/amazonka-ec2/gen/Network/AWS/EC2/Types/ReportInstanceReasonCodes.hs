{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ReportInstanceReasonCodes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ReportInstanceReasonCodes where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data ReportInstanceReasonCodes
  = InstanceStuckInState
  | NotAcceptingCredentials
  | Other
  | PasswordNotAvailable
  | PerformanceEBSVolume
  | PerformanceInstanceStore
  | PerformanceNetwork
  | PerformanceOther
  | Unresponsive
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

instance FromText ReportInstanceReasonCodes where
  parser =
    takeLowerText >>= \case
      "instance-stuck-in-state" -> pure InstanceStuckInState
      "not-accepting-credentials" -> pure NotAcceptingCredentials
      "other" -> pure Other
      "password-not-available" -> pure PasswordNotAvailable
      "performance-ebs-volume" -> pure PerformanceEBSVolume
      "performance-instance-store" -> pure PerformanceInstanceStore
      "performance-network" -> pure PerformanceNetwork
      "performance-other" -> pure PerformanceOther
      "unresponsive" -> pure Unresponsive
      e ->
        fromTextError $
          "Failure parsing ReportInstanceReasonCodes from value: '" <> e
            <> "'. Accepted values: instance-stuck-in-state, not-accepting-credentials, other, password-not-available, performance-ebs-volume, performance-instance-store, performance-network, performance-other, unresponsive"

instance ToText ReportInstanceReasonCodes where
  toText = \case
    InstanceStuckInState -> "instance-stuck-in-state"
    NotAcceptingCredentials -> "not-accepting-credentials"
    Other -> "other"
    PasswordNotAvailable -> "password-not-available"
    PerformanceEBSVolume -> "performance-ebs-volume"
    PerformanceInstanceStore -> "performance-instance-store"
    PerformanceNetwork -> "performance-network"
    PerformanceOther -> "performance-other"
    Unresponsive -> "unresponsive"

instance Hashable ReportInstanceReasonCodes

instance NFData ReportInstanceReasonCodes

instance ToByteString ReportInstanceReasonCodes

instance ToQuery ReportInstanceReasonCodes

instance ToHeader ReportInstanceReasonCodes

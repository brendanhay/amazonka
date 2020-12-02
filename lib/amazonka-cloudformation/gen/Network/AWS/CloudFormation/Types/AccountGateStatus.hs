{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.AccountGateStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.AccountGateStatus where

import Network.AWS.Prelude

data AccountGateStatus
  = AGSFailed
  | AGSSkipped
  | AGSSucceeded
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

instance FromText AccountGateStatus where
  parser =
    takeLowerText >>= \case
      "failed" -> pure AGSFailed
      "skipped" -> pure AGSSkipped
      "succeeded" -> pure AGSSucceeded
      e ->
        fromTextError $
          "Failure parsing AccountGateStatus from value: '" <> e
            <> "'. Accepted values: failed, skipped, succeeded"

instance ToText AccountGateStatus where
  toText = \case
    AGSFailed -> "FAILED"
    AGSSkipped -> "SKIPPED"
    AGSSucceeded -> "SUCCEEDED"

instance Hashable AccountGateStatus

instance NFData AccountGateStatus

instance ToByteString AccountGateStatus

instance ToQuery AccountGateStatus

instance ToHeader AccountGateStatus

instance FromXML AccountGateStatus where
  parseXML = parseXMLText "AccountGateStatus"

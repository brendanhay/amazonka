{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.BusinessReportStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.BusinessReportStatus where

import Network.AWS.Prelude

data BusinessReportStatus
  = BRSFailed
  | BRSRunning
  | BRSSucceeded
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

instance FromText BusinessReportStatus where
  parser =
    takeLowerText >>= \case
      "failed" -> pure BRSFailed
      "running" -> pure BRSRunning
      "succeeded" -> pure BRSSucceeded
      e ->
        fromTextError $
          "Failure parsing BusinessReportStatus from value: '" <> e
            <> "'. Accepted values: failed, running, succeeded"

instance ToText BusinessReportStatus where
  toText = \case
    BRSFailed -> "FAILED"
    BRSRunning -> "RUNNING"
    BRSSucceeded -> "SUCCEEDED"

instance Hashable BusinessReportStatus

instance NFData BusinessReportStatus

instance ToByteString BusinessReportStatus

instance ToQuery BusinessReportStatus

instance ToHeader BusinessReportStatus

instance FromJSON BusinessReportStatus where
  parseJSON = parseJSONText "BusinessReportStatus"

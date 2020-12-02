{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostAndUsageReport.Types.ReportVersioning
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostAndUsageReport.Types.ReportVersioning where

import Network.AWS.Prelude

data ReportVersioning
  = CreateNewReport
  | OverwriteReport
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

instance FromText ReportVersioning where
  parser =
    takeLowerText >>= \case
      "create_new_report" -> pure CreateNewReport
      "overwrite_report" -> pure OverwriteReport
      e ->
        fromTextError $
          "Failure parsing ReportVersioning from value: '" <> e
            <> "'. Accepted values: create_new_report, overwrite_report"

instance ToText ReportVersioning where
  toText = \case
    CreateNewReport -> "CREATE_NEW_REPORT"
    OverwriteReport -> "OVERWRITE_REPORT"

instance Hashable ReportVersioning

instance NFData ReportVersioning

instance ToByteString ReportVersioning

instance ToQuery ReportVersioning

instance ToHeader ReportVersioning

instance ToJSON ReportVersioning where
  toJSON = toJSONText

instance FromJSON ReportVersioning where
  parseJSON = parseJSONText "ReportVersioning"

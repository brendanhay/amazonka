{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostAndUsageReport.Types.ReportFormat
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostAndUsageReport.Types.ReportFormat where

import Network.AWS.Prelude

-- | The format that AWS saves the report in.
data ReportFormat
  = Parquet
  | TextORcsv
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

instance FromText ReportFormat where
  parser =
    takeLowerText >>= \case
      "parquet" -> pure Parquet
      "textorcsv" -> pure TextORcsv
      e ->
        fromTextError $
          "Failure parsing ReportFormat from value: '" <> e
            <> "'. Accepted values: parquet, textorcsv"

instance ToText ReportFormat where
  toText = \case
    Parquet -> "Parquet"
    TextORcsv -> "textORcsv"

instance Hashable ReportFormat

instance NFData ReportFormat

instance ToByteString ReportFormat

instance ToQuery ReportFormat

instance ToHeader ReportFormat

instance ToJSON ReportFormat where
  toJSON = toJSONText

instance FromJSON ReportFormat where
  parseJSON = parseJSONText "ReportFormat"

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.BusinessReportFormat
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.BusinessReportFormat where

import Network.AWS.Prelude

data BusinessReportFormat
  = CSV
  | CSVZip
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

instance FromText BusinessReportFormat where
  parser =
    takeLowerText >>= \case
      "csv" -> pure CSV
      "csv_zip" -> pure CSVZip
      e ->
        fromTextError $
          "Failure parsing BusinessReportFormat from value: '" <> e
            <> "'. Accepted values: csv, csv_zip"

instance ToText BusinessReportFormat where
  toText = \case
    CSV -> "CSV"
    CSVZip -> "CSV_ZIP"

instance Hashable BusinessReportFormat

instance NFData BusinessReportFormat

instance ToByteString BusinessReportFormat

instance ToQuery BusinessReportFormat

instance ToHeader BusinessReportFormat

instance ToJSON BusinessReportFormat where
  toJSON = toJSONText

instance FromJSON BusinessReportFormat where
  parseJSON = parseJSONText "BusinessReportFormat"

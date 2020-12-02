{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.AnalyticsS3ExportFileFormat
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.AnalyticsS3ExportFileFormat where

import Network.AWS.Prelude
import Network.AWS.S3.Internal

data AnalyticsS3ExportFileFormat = CSV
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

instance FromText AnalyticsS3ExportFileFormat where
  parser =
    takeLowerText >>= \case
      "csv" -> pure CSV
      e ->
        fromTextError $
          "Failure parsing AnalyticsS3ExportFileFormat from value: '" <> e
            <> "'. Accepted values: csv"

instance ToText AnalyticsS3ExportFileFormat where
  toText = \case
    CSV -> "CSV"

instance Hashable AnalyticsS3ExportFileFormat

instance NFData AnalyticsS3ExportFileFormat

instance ToByteString AnalyticsS3ExportFileFormat

instance ToQuery AnalyticsS3ExportFileFormat

instance ToHeader AnalyticsS3ExportFileFormat

instance FromXML AnalyticsS3ExportFileFormat where
  parseXML = parseXMLText "AnalyticsS3ExportFileFormat"

instance ToXML AnalyticsS3ExportFileFormat where
  toXML = toXMLText

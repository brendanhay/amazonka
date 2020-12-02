{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.ReportFormatType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.ReportFormatType where

import Network.AWS.Prelude

data ReportFormatType = TextCSV
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

instance FromText ReportFormatType where
  parser =
    takeLowerText >>= \case
      "text/csv" -> pure TextCSV
      e ->
        fromTextError $
          "Failure parsing ReportFormatType from value: '" <> e
            <> "'. Accepted values: text/csv"

instance ToText ReportFormatType where
  toText = \case
    TextCSV -> "text/csv"

instance Hashable ReportFormatType

instance NFData ReportFormatType

instance ToByteString ReportFormatType

instance ToQuery ReportFormatType

instance ToHeader ReportFormatType

instance FromXML ReportFormatType where
  parseXML = parseXMLText "ReportFormatType"

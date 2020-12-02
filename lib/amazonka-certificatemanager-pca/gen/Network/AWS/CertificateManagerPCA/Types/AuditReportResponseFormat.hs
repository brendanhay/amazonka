{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManagerPCA.Types.AuditReportResponseFormat
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManagerPCA.Types.AuditReportResponseFormat where

import Network.AWS.Prelude

data AuditReportResponseFormat
  = CSV
  | JSON
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

instance FromText AuditReportResponseFormat where
  parser =
    takeLowerText >>= \case
      "csv" -> pure CSV
      "json" -> pure JSON
      e ->
        fromTextError $
          "Failure parsing AuditReportResponseFormat from value: '" <> e
            <> "'. Accepted values: csv, json"

instance ToText AuditReportResponseFormat where
  toText = \case
    CSV -> "CSV"
    JSON -> "JSON"

instance Hashable AuditReportResponseFormat

instance NFData AuditReportResponseFormat

instance ToByteString AuditReportResponseFormat

instance ToQuery AuditReportResponseFormat

instance ToHeader AuditReportResponseFormat

instance ToJSON AuditReportResponseFormat where
  toJSON = toJSONText

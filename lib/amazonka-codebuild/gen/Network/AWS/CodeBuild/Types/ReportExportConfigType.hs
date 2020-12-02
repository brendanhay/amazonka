{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.ReportExportConfigType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.ReportExportConfigType where

import Network.AWS.Prelude

data ReportExportConfigType
  = RECTNoExport
  | RECTS3
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

instance FromText ReportExportConfigType where
  parser =
    takeLowerText >>= \case
      "no_export" -> pure RECTNoExport
      "s3" -> pure RECTS3
      e ->
        fromTextError $
          "Failure parsing ReportExportConfigType from value: '" <> e
            <> "'. Accepted values: no_export, s3"

instance ToText ReportExportConfigType where
  toText = \case
    RECTNoExport -> "NO_EXPORT"
    RECTS3 -> "S3"

instance Hashable ReportExportConfigType

instance NFData ReportExportConfigType

instance ToByteString ReportExportConfigType

instance ToQuery ReportExportConfigType

instance ToHeader ReportExportConfigType

instance ToJSON ReportExportConfigType where
  toJSON = toJSONText

instance FromJSON ReportExportConfigType where
  parseJSON = parseJSONText "ReportExportConfigType"

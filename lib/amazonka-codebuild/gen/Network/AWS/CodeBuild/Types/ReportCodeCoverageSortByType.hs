{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.ReportCodeCoverageSortByType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.ReportCodeCoverageSortByType where

import Network.AWS.Prelude

data ReportCodeCoverageSortByType
  = RCCSBTFilePath
  | RCCSBTLineCoveragePercentage
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

instance FromText ReportCodeCoverageSortByType where
  parser =
    takeLowerText >>= \case
      "file_path" -> pure RCCSBTFilePath
      "line_coverage_percentage" -> pure RCCSBTLineCoveragePercentage
      e ->
        fromTextError $
          "Failure parsing ReportCodeCoverageSortByType from value: '" <> e
            <> "'. Accepted values: file_path, line_coverage_percentage"

instance ToText ReportCodeCoverageSortByType where
  toText = \case
    RCCSBTFilePath -> "FILE_PATH"
    RCCSBTLineCoveragePercentage -> "LINE_COVERAGE_PERCENTAGE"

instance Hashable ReportCodeCoverageSortByType

instance NFData ReportCodeCoverageSortByType

instance ToByteString ReportCodeCoverageSortByType

instance ToQuery ReportCodeCoverageSortByType

instance ToHeader ReportCodeCoverageSortByType

instance ToJSON ReportCodeCoverageSortByType where
  toJSON = toJSONText

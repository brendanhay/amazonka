{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.ReportFileFormat
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.ReportFileFormat where

import Network.AWS.Prelude

data ReportFileFormat
  = HTML
  | Pdf
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

instance FromText ReportFileFormat where
  parser =
    takeLowerText >>= \case
      "html" -> pure HTML
      "pdf" -> pure Pdf
      e ->
        fromTextError $
          "Failure parsing ReportFileFormat from value: '" <> e
            <> "'. Accepted values: html, pdf"

instance ToText ReportFileFormat where
  toText = \case
    HTML -> "HTML"
    Pdf -> "PDF"

instance Hashable ReportFileFormat

instance NFData ReportFileFormat

instance ToByteString ReportFileFormat

instance ToQuery ReportFileFormat

instance ToHeader ReportFileFormat

instance ToJSON ReportFileFormat where
  toJSON = toJSONText

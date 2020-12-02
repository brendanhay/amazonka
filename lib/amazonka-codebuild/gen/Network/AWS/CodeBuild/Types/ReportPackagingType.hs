{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.ReportPackagingType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.ReportPackagingType where

import Network.AWS.Prelude

data ReportPackagingType
  = RPTNone
  | RPTZip
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

instance FromText ReportPackagingType where
  parser =
    takeLowerText >>= \case
      "none" -> pure RPTNone
      "zip" -> pure RPTZip
      e ->
        fromTextError $
          "Failure parsing ReportPackagingType from value: '" <> e
            <> "'. Accepted values: none, zip"

instance ToText ReportPackagingType where
  toText = \case
    RPTNone -> "NONE"
    RPTZip -> "ZIP"

instance Hashable ReportPackagingType

instance NFData ReportPackagingType

instance ToByteString ReportPackagingType

instance ToQuery ReportPackagingType

instance ToHeader ReportPackagingType

instance ToJSON ReportPackagingType where
  toJSON = toJSONText

instance FromJSON ReportPackagingType where
  parseJSON = parseJSONText "ReportPackagingType"

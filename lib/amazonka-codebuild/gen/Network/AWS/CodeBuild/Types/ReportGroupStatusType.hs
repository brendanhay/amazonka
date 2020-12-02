{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.ReportGroupStatusType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.ReportGroupStatusType where

import Network.AWS.Prelude

data ReportGroupStatusType
  = Active
  | Deleting
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

instance FromText ReportGroupStatusType where
  parser =
    takeLowerText >>= \case
      "active" -> pure Active
      "deleting" -> pure Deleting
      e ->
        fromTextError $
          "Failure parsing ReportGroupStatusType from value: '" <> e
            <> "'. Accepted values: active, deleting"

instance ToText ReportGroupStatusType where
  toText = \case
    Active -> "ACTIVE"
    Deleting -> "DELETING"

instance Hashable ReportGroupStatusType

instance NFData ReportGroupStatusType

instance ToByteString ReportGroupStatusType

instance ToQuery ReportGroupStatusType

instance ToHeader ReportGroupStatusType

instance FromJSON ReportGroupStatusType where
  parseJSON = parseJSONText "ReportGroupStatusType"

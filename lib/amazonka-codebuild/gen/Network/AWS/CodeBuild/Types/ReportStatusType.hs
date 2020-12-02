{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.ReportStatusType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.ReportStatusType where

import Network.AWS.Prelude

data ReportStatusType
  = RSTDeleting
  | RSTFailed
  | RSTGenerating
  | RSTIncomplete
  | RSTSucceeded
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

instance FromText ReportStatusType where
  parser =
    takeLowerText >>= \case
      "deleting" -> pure RSTDeleting
      "failed" -> pure RSTFailed
      "generating" -> pure RSTGenerating
      "incomplete" -> pure RSTIncomplete
      "succeeded" -> pure RSTSucceeded
      e ->
        fromTextError $
          "Failure parsing ReportStatusType from value: '" <> e
            <> "'. Accepted values: deleting, failed, generating, incomplete, succeeded"

instance ToText ReportStatusType where
  toText = \case
    RSTDeleting -> "DELETING"
    RSTFailed -> "FAILED"
    RSTGenerating -> "GENERATING"
    RSTIncomplete -> "INCOMPLETE"
    RSTSucceeded -> "SUCCEEDED"

instance Hashable ReportStatusType

instance NFData ReportStatusType

instance ToByteString ReportStatusType

instance ToQuery ReportStatusType

instance ToHeader ReportStatusType

instance ToJSON ReportStatusType where
  toJSON = toJSONText

instance FromJSON ReportStatusType where
  parseJSON = parseJSONText "ReportStatusType"

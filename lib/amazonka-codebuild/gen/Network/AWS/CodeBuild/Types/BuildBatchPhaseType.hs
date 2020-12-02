{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.BuildBatchPhaseType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.BuildBatchPhaseType where

import Network.AWS.Prelude

data BuildBatchPhaseType
  = BBPTCombineArtifacts
  | BBPTDownloadBatchspec
  | BBPTFailed
  | BBPTInProgress
  | BBPTStopped
  | BBPTSubmitted
  | BBPTSucceeded
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

instance FromText BuildBatchPhaseType where
  parser =
    takeLowerText >>= \case
      "combine_artifacts" -> pure BBPTCombineArtifacts
      "download_batchspec" -> pure BBPTDownloadBatchspec
      "failed" -> pure BBPTFailed
      "in_progress" -> pure BBPTInProgress
      "stopped" -> pure BBPTStopped
      "submitted" -> pure BBPTSubmitted
      "succeeded" -> pure BBPTSucceeded
      e ->
        fromTextError $
          "Failure parsing BuildBatchPhaseType from value: '" <> e
            <> "'. Accepted values: combine_artifacts, download_batchspec, failed, in_progress, stopped, submitted, succeeded"

instance ToText BuildBatchPhaseType where
  toText = \case
    BBPTCombineArtifacts -> "COMBINE_ARTIFACTS"
    BBPTDownloadBatchspec -> "DOWNLOAD_BATCHSPEC"
    BBPTFailed -> "FAILED"
    BBPTInProgress -> "IN_PROGRESS"
    BBPTStopped -> "STOPPED"
    BBPTSubmitted -> "SUBMITTED"
    BBPTSucceeded -> "SUCCEEDED"

instance Hashable BuildBatchPhaseType

instance NFData BuildBatchPhaseType

instance ToByteString BuildBatchPhaseType

instance ToQuery BuildBatchPhaseType

instance ToHeader BuildBatchPhaseType

instance FromJSON BuildBatchPhaseType where
  parseJSON = parseJSONText "BuildBatchPhaseType"

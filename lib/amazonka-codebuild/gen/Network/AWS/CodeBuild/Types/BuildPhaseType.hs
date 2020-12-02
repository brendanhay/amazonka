{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.BuildPhaseType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.BuildPhaseType where

import Network.AWS.Prelude

data BuildPhaseType
  = BPTBuild
  | BPTCompleted
  | BPTDownloadSource
  | BPTFinalizing
  | BPTInstall
  | BPTPostBuild
  | BPTPreBuild
  | BPTProvisioning
  | BPTQueued
  | BPTSubmitted
  | BPTUploadArtifacts
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

instance FromText BuildPhaseType where
  parser =
    takeLowerText >>= \case
      "build" -> pure BPTBuild
      "completed" -> pure BPTCompleted
      "download_source" -> pure BPTDownloadSource
      "finalizing" -> pure BPTFinalizing
      "install" -> pure BPTInstall
      "post_build" -> pure BPTPostBuild
      "pre_build" -> pure BPTPreBuild
      "provisioning" -> pure BPTProvisioning
      "queued" -> pure BPTQueued
      "submitted" -> pure BPTSubmitted
      "upload_artifacts" -> pure BPTUploadArtifacts
      e ->
        fromTextError $
          "Failure parsing BuildPhaseType from value: '" <> e
            <> "'. Accepted values: build, completed, download_source, finalizing, install, post_build, pre_build, provisioning, queued, submitted, upload_artifacts"

instance ToText BuildPhaseType where
  toText = \case
    BPTBuild -> "BUILD"
    BPTCompleted -> "COMPLETED"
    BPTDownloadSource -> "DOWNLOAD_SOURCE"
    BPTFinalizing -> "FINALIZING"
    BPTInstall -> "INSTALL"
    BPTPostBuild -> "POST_BUILD"
    BPTPreBuild -> "PRE_BUILD"
    BPTProvisioning -> "PROVISIONING"
    BPTQueued -> "QUEUED"
    BPTSubmitted -> "SUBMITTED"
    BPTUploadArtifacts -> "UPLOAD_ARTIFACTS"

instance Hashable BuildPhaseType

instance NFData BuildPhaseType

instance ToByteString BuildPhaseType

instance ToQuery BuildPhaseType

instance ToHeader BuildPhaseType

instance FromJSON BuildPhaseType where
  parseJSON = parseJSONText "BuildPhaseType"

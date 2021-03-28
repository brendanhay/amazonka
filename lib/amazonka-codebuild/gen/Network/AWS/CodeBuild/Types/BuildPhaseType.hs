{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.BuildPhaseType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeBuild.Types.BuildPhaseType
  ( BuildPhaseType
    ( BuildPhaseType'
    , BuildPhaseTypeSubmitted
    , BuildPhaseTypeQueued
    , BuildPhaseTypeProvisioning
    , BuildPhaseTypeDownloadSource
    , BuildPhaseTypeInstall
    , BuildPhaseTypePreBuild
    , BuildPhaseTypeBuild
    , BuildPhaseTypePostBuild
    , BuildPhaseTypeUploadArtifacts
    , BuildPhaseTypeFinalizing
    , BuildPhaseTypeCompleted
    , fromBuildPhaseType
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype BuildPhaseType = BuildPhaseType'{fromBuildPhaseType ::
                                         Core.Text}
                           deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                           Core.Generic)
                           deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                             Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                             Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                             Core.FromText, Core.ToByteString, Core.ToQuery,
                                             Core.ToHeader)

pattern BuildPhaseTypeSubmitted :: BuildPhaseType
pattern BuildPhaseTypeSubmitted = BuildPhaseType' "SUBMITTED"

pattern BuildPhaseTypeQueued :: BuildPhaseType
pattern BuildPhaseTypeQueued = BuildPhaseType' "QUEUED"

pattern BuildPhaseTypeProvisioning :: BuildPhaseType
pattern BuildPhaseTypeProvisioning = BuildPhaseType' "PROVISIONING"

pattern BuildPhaseTypeDownloadSource :: BuildPhaseType
pattern BuildPhaseTypeDownloadSource = BuildPhaseType' "DOWNLOAD_SOURCE"

pattern BuildPhaseTypeInstall :: BuildPhaseType
pattern BuildPhaseTypeInstall = BuildPhaseType' "INSTALL"

pattern BuildPhaseTypePreBuild :: BuildPhaseType
pattern BuildPhaseTypePreBuild = BuildPhaseType' "PRE_BUILD"

pattern BuildPhaseTypeBuild :: BuildPhaseType
pattern BuildPhaseTypeBuild = BuildPhaseType' "BUILD"

pattern BuildPhaseTypePostBuild :: BuildPhaseType
pattern BuildPhaseTypePostBuild = BuildPhaseType' "POST_BUILD"

pattern BuildPhaseTypeUploadArtifacts :: BuildPhaseType
pattern BuildPhaseTypeUploadArtifacts = BuildPhaseType' "UPLOAD_ARTIFACTS"

pattern BuildPhaseTypeFinalizing :: BuildPhaseType
pattern BuildPhaseTypeFinalizing = BuildPhaseType' "FINALIZING"

pattern BuildPhaseTypeCompleted :: BuildPhaseType
pattern BuildPhaseTypeCompleted = BuildPhaseType' "COMPLETED"

{-# COMPLETE 
  BuildPhaseTypeSubmitted,

  BuildPhaseTypeQueued,

  BuildPhaseTypeProvisioning,

  BuildPhaseTypeDownloadSource,

  BuildPhaseTypeInstall,

  BuildPhaseTypePreBuild,

  BuildPhaseTypeBuild,

  BuildPhaseTypePostBuild,

  BuildPhaseTypeUploadArtifacts,

  BuildPhaseTypeFinalizing,

  BuildPhaseTypeCompleted,
  BuildPhaseType'
  #-}

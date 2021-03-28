{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.BuildBatchPhaseType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeBuild.Types.BuildBatchPhaseType
  ( BuildBatchPhaseType
    ( BuildBatchPhaseType'
    , BuildBatchPhaseTypeSubmitted
    , BuildBatchPhaseTypeDownloadBatchspec
    , BuildBatchPhaseTypeInProgress
    , BuildBatchPhaseTypeCombineArtifacts
    , BuildBatchPhaseTypeSucceeded
    , BuildBatchPhaseTypeFailed
    , BuildBatchPhaseTypeStopped
    , fromBuildBatchPhaseType
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype BuildBatchPhaseType = BuildBatchPhaseType'{fromBuildBatchPhaseType
                                                   :: Core.Text}
                                deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                Core.Generic)
                                deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                  Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                  Core.FromJSON, Core.ToXML, Core.FromXML,
                                                  Core.ToText, Core.FromText, Core.ToByteString,
                                                  Core.ToQuery, Core.ToHeader)

pattern BuildBatchPhaseTypeSubmitted :: BuildBatchPhaseType
pattern BuildBatchPhaseTypeSubmitted = BuildBatchPhaseType' "SUBMITTED"

pattern BuildBatchPhaseTypeDownloadBatchspec :: BuildBatchPhaseType
pattern BuildBatchPhaseTypeDownloadBatchspec = BuildBatchPhaseType' "DOWNLOAD_BATCHSPEC"

pattern BuildBatchPhaseTypeInProgress :: BuildBatchPhaseType
pattern BuildBatchPhaseTypeInProgress = BuildBatchPhaseType' "IN_PROGRESS"

pattern BuildBatchPhaseTypeCombineArtifacts :: BuildBatchPhaseType
pattern BuildBatchPhaseTypeCombineArtifacts = BuildBatchPhaseType' "COMBINE_ARTIFACTS"

pattern BuildBatchPhaseTypeSucceeded :: BuildBatchPhaseType
pattern BuildBatchPhaseTypeSucceeded = BuildBatchPhaseType' "SUCCEEDED"

pattern BuildBatchPhaseTypeFailed :: BuildBatchPhaseType
pattern BuildBatchPhaseTypeFailed = BuildBatchPhaseType' "FAILED"

pattern BuildBatchPhaseTypeStopped :: BuildBatchPhaseType
pattern BuildBatchPhaseTypeStopped = BuildBatchPhaseType' "STOPPED"

{-# COMPLETE 
  BuildBatchPhaseTypeSubmitted,

  BuildBatchPhaseTypeDownloadBatchspec,

  BuildBatchPhaseTypeInProgress,

  BuildBatchPhaseTypeCombineArtifacts,

  BuildBatchPhaseTypeSucceeded,

  BuildBatchPhaseTypeFailed,

  BuildBatchPhaseTypeStopped,
  BuildBatchPhaseType'
  #-}

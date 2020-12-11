-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.BuildBatchPhaseType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.BuildBatchPhaseType
  ( BuildBatchPhaseType
      ( BuildBatchPhaseType',
        BBPTCombineArtifacts,
        BBPTDownloadBatchspec,
        BBPTFailed,
        BBPTInProgress,
        BBPTStopped,
        BBPTSubmitted,
        BBPTSucceeded
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype BuildBatchPhaseType = BuildBatchPhaseType' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern BBPTCombineArtifacts :: BuildBatchPhaseType
pattern BBPTCombineArtifacts = BuildBatchPhaseType' "COMBINE_ARTIFACTS"

pattern BBPTDownloadBatchspec :: BuildBatchPhaseType
pattern BBPTDownloadBatchspec = BuildBatchPhaseType' "DOWNLOAD_BATCHSPEC"

pattern BBPTFailed :: BuildBatchPhaseType
pattern BBPTFailed = BuildBatchPhaseType' "FAILED"

pattern BBPTInProgress :: BuildBatchPhaseType
pattern BBPTInProgress = BuildBatchPhaseType' "IN_PROGRESS"

pattern BBPTStopped :: BuildBatchPhaseType
pattern BBPTStopped = BuildBatchPhaseType' "STOPPED"

pattern BBPTSubmitted :: BuildBatchPhaseType
pattern BBPTSubmitted = BuildBatchPhaseType' "SUBMITTED"

pattern BBPTSucceeded :: BuildBatchPhaseType
pattern BBPTSucceeded = BuildBatchPhaseType' "SUCCEEDED"

{-# COMPLETE
  BBPTCombineArtifacts,
  BBPTDownloadBatchspec,
  BBPTFailed,
  BBPTInProgress,
  BBPTStopped,
  BBPTSubmitted,
  BBPTSucceeded,
  BuildBatchPhaseType'
  #-}

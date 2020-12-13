{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.BuildPhaseType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.BuildPhaseType
  ( BuildPhaseType
      ( BuildPhaseType',
        BPTSubmitted,
        BPTQueued,
        BPTProvisioning,
        BPTDownloadSource,
        BPTInstall,
        BPTPreBuild,
        BPTBuild,
        BPTPostBuild,
        BPTUploadArtifacts,
        BPTFinalizing,
        BPTCompleted
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype BuildPhaseType = BuildPhaseType' Lude.Text
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

pattern BPTSubmitted :: BuildPhaseType
pattern BPTSubmitted = BuildPhaseType' "SUBMITTED"

pattern BPTQueued :: BuildPhaseType
pattern BPTQueued = BuildPhaseType' "QUEUED"

pattern BPTProvisioning :: BuildPhaseType
pattern BPTProvisioning = BuildPhaseType' "PROVISIONING"

pattern BPTDownloadSource :: BuildPhaseType
pattern BPTDownloadSource = BuildPhaseType' "DOWNLOAD_SOURCE"

pattern BPTInstall :: BuildPhaseType
pattern BPTInstall = BuildPhaseType' "INSTALL"

pattern BPTPreBuild :: BuildPhaseType
pattern BPTPreBuild = BuildPhaseType' "PRE_BUILD"

pattern BPTBuild :: BuildPhaseType
pattern BPTBuild = BuildPhaseType' "BUILD"

pattern BPTPostBuild :: BuildPhaseType
pattern BPTPostBuild = BuildPhaseType' "POST_BUILD"

pattern BPTUploadArtifacts :: BuildPhaseType
pattern BPTUploadArtifacts = BuildPhaseType' "UPLOAD_ARTIFACTS"

pattern BPTFinalizing :: BuildPhaseType
pattern BPTFinalizing = BuildPhaseType' "FINALIZING"

pattern BPTCompleted :: BuildPhaseType
pattern BPTCompleted = BuildPhaseType' "COMPLETED"

{-# COMPLETE
  BPTSubmitted,
  BPTQueued,
  BPTProvisioning,
  BPTDownloadSource,
  BPTInstall,
  BPTPreBuild,
  BPTBuild,
  BPTPostBuild,
  BPTUploadArtifacts,
  BPTFinalizing,
  BPTCompleted,
  BuildPhaseType'
  #-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.BuildPhaseType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.BuildPhaseType
  ( BuildPhaseType
      ( ..,
        BuildPhaseType_BUILD,
        BuildPhaseType_COMPLETED,
        BuildPhaseType_DOWNLOAD_SOURCE,
        BuildPhaseType_FINALIZING,
        BuildPhaseType_INSTALL,
        BuildPhaseType_POST_BUILD,
        BuildPhaseType_PRE_BUILD,
        BuildPhaseType_PROVISIONING,
        BuildPhaseType_QUEUED,
        BuildPhaseType_SUBMITTED,
        BuildPhaseType_UPLOAD_ARTIFACTS
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype BuildPhaseType = BuildPhaseType'
  { fromBuildPhaseType ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern BuildPhaseType_BUILD :: BuildPhaseType
pattern BuildPhaseType_BUILD = BuildPhaseType' "BUILD"

pattern BuildPhaseType_COMPLETED :: BuildPhaseType
pattern BuildPhaseType_COMPLETED = BuildPhaseType' "COMPLETED"

pattern BuildPhaseType_DOWNLOAD_SOURCE :: BuildPhaseType
pattern BuildPhaseType_DOWNLOAD_SOURCE = BuildPhaseType' "DOWNLOAD_SOURCE"

pattern BuildPhaseType_FINALIZING :: BuildPhaseType
pattern BuildPhaseType_FINALIZING = BuildPhaseType' "FINALIZING"

pattern BuildPhaseType_INSTALL :: BuildPhaseType
pattern BuildPhaseType_INSTALL = BuildPhaseType' "INSTALL"

pattern BuildPhaseType_POST_BUILD :: BuildPhaseType
pattern BuildPhaseType_POST_BUILD = BuildPhaseType' "POST_BUILD"

pattern BuildPhaseType_PRE_BUILD :: BuildPhaseType
pattern BuildPhaseType_PRE_BUILD = BuildPhaseType' "PRE_BUILD"

pattern BuildPhaseType_PROVISIONING :: BuildPhaseType
pattern BuildPhaseType_PROVISIONING = BuildPhaseType' "PROVISIONING"

pattern BuildPhaseType_QUEUED :: BuildPhaseType
pattern BuildPhaseType_QUEUED = BuildPhaseType' "QUEUED"

pattern BuildPhaseType_SUBMITTED :: BuildPhaseType
pattern BuildPhaseType_SUBMITTED = BuildPhaseType' "SUBMITTED"

pattern BuildPhaseType_UPLOAD_ARTIFACTS :: BuildPhaseType
pattern BuildPhaseType_UPLOAD_ARTIFACTS = BuildPhaseType' "UPLOAD_ARTIFACTS"

{-# COMPLETE
  BuildPhaseType_BUILD,
  BuildPhaseType_COMPLETED,
  BuildPhaseType_DOWNLOAD_SOURCE,
  BuildPhaseType_FINALIZING,
  BuildPhaseType_INSTALL,
  BuildPhaseType_POST_BUILD,
  BuildPhaseType_PRE_BUILD,
  BuildPhaseType_PROVISIONING,
  BuildPhaseType_QUEUED,
  BuildPhaseType_SUBMITTED,
  BuildPhaseType_UPLOAD_ARTIFACTS,
  BuildPhaseType'
  #-}

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
-- Module      : Amazonka.CodeBuild.Types.BuildPhaseType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeBuild.Types.BuildPhaseType
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype BuildPhaseType = BuildPhaseType'
  { fromBuildPhaseType ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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

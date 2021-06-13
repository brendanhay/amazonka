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
-- Module      : Network.AWS.CodeBuild.Types.BuildBatchPhaseType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.BuildBatchPhaseType
  ( BuildBatchPhaseType
      ( ..,
        BuildBatchPhaseType_COMBINE_ARTIFACTS,
        BuildBatchPhaseType_DOWNLOAD_BATCHSPEC,
        BuildBatchPhaseType_FAILED,
        BuildBatchPhaseType_IN_PROGRESS,
        BuildBatchPhaseType_STOPPED,
        BuildBatchPhaseType_SUBMITTED,
        BuildBatchPhaseType_SUCCEEDED
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype BuildBatchPhaseType = BuildBatchPhaseType'
  { fromBuildBatchPhaseType ::
      Core.Text
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

pattern BuildBatchPhaseType_COMBINE_ARTIFACTS :: BuildBatchPhaseType
pattern BuildBatchPhaseType_COMBINE_ARTIFACTS = BuildBatchPhaseType' "COMBINE_ARTIFACTS"

pattern BuildBatchPhaseType_DOWNLOAD_BATCHSPEC :: BuildBatchPhaseType
pattern BuildBatchPhaseType_DOWNLOAD_BATCHSPEC = BuildBatchPhaseType' "DOWNLOAD_BATCHSPEC"

pattern BuildBatchPhaseType_FAILED :: BuildBatchPhaseType
pattern BuildBatchPhaseType_FAILED = BuildBatchPhaseType' "FAILED"

pattern BuildBatchPhaseType_IN_PROGRESS :: BuildBatchPhaseType
pattern BuildBatchPhaseType_IN_PROGRESS = BuildBatchPhaseType' "IN_PROGRESS"

pattern BuildBatchPhaseType_STOPPED :: BuildBatchPhaseType
pattern BuildBatchPhaseType_STOPPED = BuildBatchPhaseType' "STOPPED"

pattern BuildBatchPhaseType_SUBMITTED :: BuildBatchPhaseType
pattern BuildBatchPhaseType_SUBMITTED = BuildBatchPhaseType' "SUBMITTED"

pattern BuildBatchPhaseType_SUCCEEDED :: BuildBatchPhaseType
pattern BuildBatchPhaseType_SUCCEEDED = BuildBatchPhaseType' "SUCCEEDED"

{-# COMPLETE
  BuildBatchPhaseType_COMBINE_ARTIFACTS,
  BuildBatchPhaseType_DOWNLOAD_BATCHSPEC,
  BuildBatchPhaseType_FAILED,
  BuildBatchPhaseType_IN_PROGRESS,
  BuildBatchPhaseType_STOPPED,
  BuildBatchPhaseType_SUBMITTED,
  BuildBatchPhaseType_SUCCEEDED,
  BuildBatchPhaseType'
  #-}

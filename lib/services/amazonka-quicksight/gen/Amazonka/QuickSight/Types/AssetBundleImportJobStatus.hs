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
-- Module      : Amazonka.QuickSight.Types.AssetBundleImportJobStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.AssetBundleImportJobStatus
  ( AssetBundleImportJobStatus
      ( ..,
        AssetBundleImportJobStatus_FAILED,
        AssetBundleImportJobStatus_FAILED_ROLLBACK_COMPLETED,
        AssetBundleImportJobStatus_FAILED_ROLLBACK_ERROR,
        AssetBundleImportJobStatus_FAILED_ROLLBACK_IN_PROGRESS,
        AssetBundleImportJobStatus_IN_PROGRESS,
        AssetBundleImportJobStatus_QUEUED_FOR_IMMEDIATE_EXECUTION,
        AssetBundleImportJobStatus_SUCCESSFUL
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AssetBundleImportJobStatus = AssetBundleImportJobStatus'
  { fromAssetBundleImportJobStatus ::
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

pattern AssetBundleImportJobStatus_FAILED :: AssetBundleImportJobStatus
pattern AssetBundleImportJobStatus_FAILED = AssetBundleImportJobStatus' "FAILED"

pattern AssetBundleImportJobStatus_FAILED_ROLLBACK_COMPLETED :: AssetBundleImportJobStatus
pattern AssetBundleImportJobStatus_FAILED_ROLLBACK_COMPLETED = AssetBundleImportJobStatus' "FAILED_ROLLBACK_COMPLETED"

pattern AssetBundleImportJobStatus_FAILED_ROLLBACK_ERROR :: AssetBundleImportJobStatus
pattern AssetBundleImportJobStatus_FAILED_ROLLBACK_ERROR = AssetBundleImportJobStatus' "FAILED_ROLLBACK_ERROR"

pattern AssetBundleImportJobStatus_FAILED_ROLLBACK_IN_PROGRESS :: AssetBundleImportJobStatus
pattern AssetBundleImportJobStatus_FAILED_ROLLBACK_IN_PROGRESS = AssetBundleImportJobStatus' "FAILED_ROLLBACK_IN_PROGRESS"

pattern AssetBundleImportJobStatus_IN_PROGRESS :: AssetBundleImportJobStatus
pattern AssetBundleImportJobStatus_IN_PROGRESS = AssetBundleImportJobStatus' "IN_PROGRESS"

pattern AssetBundleImportJobStatus_QUEUED_FOR_IMMEDIATE_EXECUTION :: AssetBundleImportJobStatus
pattern AssetBundleImportJobStatus_QUEUED_FOR_IMMEDIATE_EXECUTION = AssetBundleImportJobStatus' "QUEUED_FOR_IMMEDIATE_EXECUTION"

pattern AssetBundleImportJobStatus_SUCCESSFUL :: AssetBundleImportJobStatus
pattern AssetBundleImportJobStatus_SUCCESSFUL = AssetBundleImportJobStatus' "SUCCESSFUL"

{-# COMPLETE
  AssetBundleImportJobStatus_FAILED,
  AssetBundleImportJobStatus_FAILED_ROLLBACK_COMPLETED,
  AssetBundleImportJobStatus_FAILED_ROLLBACK_ERROR,
  AssetBundleImportJobStatus_FAILED_ROLLBACK_IN_PROGRESS,
  AssetBundleImportJobStatus_IN_PROGRESS,
  AssetBundleImportJobStatus_QUEUED_FOR_IMMEDIATE_EXECUTION,
  AssetBundleImportJobStatus_SUCCESSFUL,
  AssetBundleImportJobStatus'
  #-}

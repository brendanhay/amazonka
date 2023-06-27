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
-- Module      : Amazonka.QuickSight.Types.AssetBundleExportJobStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.AssetBundleExportJobStatus
  ( AssetBundleExportJobStatus
      ( ..,
        AssetBundleExportJobStatus_FAILED,
        AssetBundleExportJobStatus_IN_PROGRESS,
        AssetBundleExportJobStatus_QUEUED_FOR_IMMEDIATE_EXECUTION,
        AssetBundleExportJobStatus_SUCCESSFUL
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AssetBundleExportJobStatus = AssetBundleExportJobStatus'
  { fromAssetBundleExportJobStatus ::
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

pattern AssetBundleExportJobStatus_FAILED :: AssetBundleExportJobStatus
pattern AssetBundleExportJobStatus_FAILED = AssetBundleExportJobStatus' "FAILED"

pattern AssetBundleExportJobStatus_IN_PROGRESS :: AssetBundleExportJobStatus
pattern AssetBundleExportJobStatus_IN_PROGRESS = AssetBundleExportJobStatus' "IN_PROGRESS"

pattern AssetBundleExportJobStatus_QUEUED_FOR_IMMEDIATE_EXECUTION :: AssetBundleExportJobStatus
pattern AssetBundleExportJobStatus_QUEUED_FOR_IMMEDIATE_EXECUTION = AssetBundleExportJobStatus' "QUEUED_FOR_IMMEDIATE_EXECUTION"

pattern AssetBundleExportJobStatus_SUCCESSFUL :: AssetBundleExportJobStatus
pattern AssetBundleExportJobStatus_SUCCESSFUL = AssetBundleExportJobStatus' "SUCCESSFUL"

{-# COMPLETE
  AssetBundleExportJobStatus_FAILED,
  AssetBundleExportJobStatus_IN_PROGRESS,
  AssetBundleExportJobStatus_QUEUED_FOR_IMMEDIATE_EXECUTION,
  AssetBundleExportJobStatus_SUCCESSFUL,
  AssetBundleExportJobStatus'
  #-}

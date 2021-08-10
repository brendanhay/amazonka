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
-- Module      : Network.AWS.Discovery.Types.ContinuousExportStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Discovery.Types.ContinuousExportStatus
  ( ContinuousExportStatus
      ( ..,
        ContinuousExportStatus_ACTIVE,
        ContinuousExportStatus_ERROR,
        ContinuousExportStatus_INACTIVE,
        ContinuousExportStatus_START_FAILED,
        ContinuousExportStatus_START_IN_PROGRESS,
        ContinuousExportStatus_STOP_FAILED,
        ContinuousExportStatus_STOP_IN_PROGRESS
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype ContinuousExportStatus = ContinuousExportStatus'
  { fromContinuousExportStatus ::
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

pattern ContinuousExportStatus_ACTIVE :: ContinuousExportStatus
pattern ContinuousExportStatus_ACTIVE = ContinuousExportStatus' "ACTIVE"

pattern ContinuousExportStatus_ERROR :: ContinuousExportStatus
pattern ContinuousExportStatus_ERROR = ContinuousExportStatus' "ERROR"

pattern ContinuousExportStatus_INACTIVE :: ContinuousExportStatus
pattern ContinuousExportStatus_INACTIVE = ContinuousExportStatus' "INACTIVE"

pattern ContinuousExportStatus_START_FAILED :: ContinuousExportStatus
pattern ContinuousExportStatus_START_FAILED = ContinuousExportStatus' "START_FAILED"

pattern ContinuousExportStatus_START_IN_PROGRESS :: ContinuousExportStatus
pattern ContinuousExportStatus_START_IN_PROGRESS = ContinuousExportStatus' "START_IN_PROGRESS"

pattern ContinuousExportStatus_STOP_FAILED :: ContinuousExportStatus
pattern ContinuousExportStatus_STOP_FAILED = ContinuousExportStatus' "STOP_FAILED"

pattern ContinuousExportStatus_STOP_IN_PROGRESS :: ContinuousExportStatus
pattern ContinuousExportStatus_STOP_IN_PROGRESS = ContinuousExportStatus' "STOP_IN_PROGRESS"

{-# COMPLETE
  ContinuousExportStatus_ACTIVE,
  ContinuousExportStatus_ERROR,
  ContinuousExportStatus_INACTIVE,
  ContinuousExportStatus_START_FAILED,
  ContinuousExportStatus_START_IN_PROGRESS,
  ContinuousExportStatus_STOP_FAILED,
  ContinuousExportStatus_STOP_IN_PROGRESS,
  ContinuousExportStatus'
  #-}

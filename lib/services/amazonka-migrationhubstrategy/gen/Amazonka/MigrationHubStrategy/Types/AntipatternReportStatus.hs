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
-- Module      : Amazonka.MigrationHubStrategy.Types.AntipatternReportStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubStrategy.Types.AntipatternReportStatus
  ( AntipatternReportStatus
      ( ..,
        AntipatternReportStatus_FAILED,
        AntipatternReportStatus_IN_PROGRESS,
        AntipatternReportStatus_SUCCESS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype AntipatternReportStatus = AntipatternReportStatus'
  { fromAntipatternReportStatus ::
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

pattern AntipatternReportStatus_FAILED :: AntipatternReportStatus
pattern AntipatternReportStatus_FAILED = AntipatternReportStatus' "FAILED"

pattern AntipatternReportStatus_IN_PROGRESS :: AntipatternReportStatus
pattern AntipatternReportStatus_IN_PROGRESS = AntipatternReportStatus' "IN_PROGRESS"

pattern AntipatternReportStatus_SUCCESS :: AntipatternReportStatus
pattern AntipatternReportStatus_SUCCESS = AntipatternReportStatus' "SUCCESS"

{-# COMPLETE
  AntipatternReportStatus_FAILED,
  AntipatternReportStatus_IN_PROGRESS,
  AntipatternReportStatus_SUCCESS,
  AntipatternReportStatus'
  #-}

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
-- Module      : Amazonka.Inspector.Types.ReportStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector.Types.ReportStatus
  ( ReportStatus
      ( ..,
        ReportStatus_COMPLETED,
        ReportStatus_FAILED,
        ReportStatus_WORK_IN_PROGRESS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype ReportStatus = ReportStatus'
  { fromReportStatus ::
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

pattern ReportStatus_COMPLETED :: ReportStatus
pattern ReportStatus_COMPLETED = ReportStatus' "COMPLETED"

pattern ReportStatus_FAILED :: ReportStatus
pattern ReportStatus_FAILED = ReportStatus' "FAILED"

pattern ReportStatus_WORK_IN_PROGRESS :: ReportStatus
pattern ReportStatus_WORK_IN_PROGRESS = ReportStatus' "WORK_IN_PROGRESS"

{-# COMPLETE
  ReportStatus_COMPLETED,
  ReportStatus_FAILED,
  ReportStatus_WORK_IN_PROGRESS,
  ReportStatus'
  #-}

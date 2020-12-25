{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostAndUsageReport.Types.ReportVersioning
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostAndUsageReport.Types.ReportVersioning
  ( ReportVersioning
      ( ReportVersioning',
        ReportVersioningCreateNewReport,
        ReportVersioningOverwriteReport,
        fromReportVersioning
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype ReportVersioning = ReportVersioning'
  { fromReportVersioning ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern ReportVersioningCreateNewReport :: ReportVersioning
pattern ReportVersioningCreateNewReport = ReportVersioning' "CREATE_NEW_REPORT"

pattern ReportVersioningOverwriteReport :: ReportVersioning
pattern ReportVersioningOverwriteReport = ReportVersioning' "OVERWRITE_REPORT"

{-# COMPLETE
  ReportVersioningCreateNewReport,
  ReportVersioningOverwriteReport,
  ReportVersioning'
  #-}

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
        CreateNewReport,
        OverwriteReport
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ReportVersioning = ReportVersioning' Lude.Text
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

pattern CreateNewReport :: ReportVersioning
pattern CreateNewReport = ReportVersioning' "CREATE_NEW_REPORT"

pattern OverwriteReport :: ReportVersioning
pattern OverwriteReport = ReportVersioning' "OVERWRITE_REPORT"

{-# COMPLETE
  CreateNewReport,
  OverwriteReport,
  ReportVersioning'
  #-}

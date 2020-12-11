-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.ReportStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.ReportStatus
  ( ReportStatus
      ( ReportStatus',
        RSCompleted,
        RSFailed,
        RSWorkInProgress
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ReportStatus = ReportStatus' Lude.Text
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

pattern RSCompleted :: ReportStatus
pattern RSCompleted = ReportStatus' "COMPLETED"

pattern RSFailed :: ReportStatus
pattern RSFailed = ReportStatus' "FAILED"

pattern RSWorkInProgress :: ReportStatus
pattern RSWorkInProgress = ReportStatus' "WORK_IN_PROGRESS"

{-# COMPLETE
  RSCompleted,
  RSFailed,
  RSWorkInProgress,
  ReportStatus'
  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.ReportStatusType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.ReportStatusType
  ( ReportStatusType
      ( ReportStatusType',
        ReportStatusTypeGenerating,
        ReportStatusTypeSucceeded,
        ReportStatusTypeFailed,
        ReportStatusTypeIncomplete,
        ReportStatusTypeDeleting,
        fromReportStatusType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype ReportStatusType = ReportStatusType'
  { fromReportStatusType ::
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

pattern ReportStatusTypeGenerating :: ReportStatusType
pattern ReportStatusTypeGenerating = ReportStatusType' "GENERATING"

pattern ReportStatusTypeSucceeded :: ReportStatusType
pattern ReportStatusTypeSucceeded = ReportStatusType' "SUCCEEDED"

pattern ReportStatusTypeFailed :: ReportStatusType
pattern ReportStatusTypeFailed = ReportStatusType' "FAILED"

pattern ReportStatusTypeIncomplete :: ReportStatusType
pattern ReportStatusTypeIncomplete = ReportStatusType' "INCOMPLETE"

pattern ReportStatusTypeDeleting :: ReportStatusType
pattern ReportStatusTypeDeleting = ReportStatusType' "DELETING"

{-# COMPLETE
  ReportStatusTypeGenerating,
  ReportStatusTypeSucceeded,
  ReportStatusTypeFailed,
  ReportStatusTypeIncomplete,
  ReportStatusTypeDeleting,
  ReportStatusType'
  #-}

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
        RSTGenerating,
        RSTSucceeded,
        RSTFailed,
        RSTIncomplete,
        RSTDeleting
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ReportStatusType = ReportStatusType' Lude.Text
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

pattern RSTGenerating :: ReportStatusType
pattern RSTGenerating = ReportStatusType' "GENERATING"

pattern RSTSucceeded :: ReportStatusType
pattern RSTSucceeded = ReportStatusType' "SUCCEEDED"

pattern RSTFailed :: ReportStatusType
pattern RSTFailed = ReportStatusType' "FAILED"

pattern RSTIncomplete :: ReportStatusType
pattern RSTIncomplete = ReportStatusType' "INCOMPLETE"

pattern RSTDeleting :: ReportStatusType
pattern RSTDeleting = ReportStatusType' "DELETING"

{-# COMPLETE
  RSTGenerating,
  RSTSucceeded,
  RSTFailed,
  RSTIncomplete,
  RSTDeleting,
  ReportStatusType'
  #-}

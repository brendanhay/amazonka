{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.InspectorEvent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.InspectorEvent
  ( InspectorEvent
      ( InspectorEvent',
        AssessmentRunStarted,
        AssessmentRunCompleted,
        AssessmentRunStateChanged,
        FindingReported,
        Other
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype InspectorEvent = InspectorEvent' Lude.Text
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

pattern AssessmentRunStarted :: InspectorEvent
pattern AssessmentRunStarted = InspectorEvent' "ASSESSMENT_RUN_STARTED"

pattern AssessmentRunCompleted :: InspectorEvent
pattern AssessmentRunCompleted = InspectorEvent' "ASSESSMENT_RUN_COMPLETED"

pattern AssessmentRunStateChanged :: InspectorEvent
pattern AssessmentRunStateChanged = InspectorEvent' "ASSESSMENT_RUN_STATE_CHANGED"

pattern FindingReported :: InspectorEvent
pattern FindingReported = InspectorEvent' "FINDING_REPORTED"

pattern Other :: InspectorEvent
pattern Other = InspectorEvent' "OTHER"

{-# COMPLETE
  AssessmentRunStarted,
  AssessmentRunCompleted,
  AssessmentRunStateChanged,
  FindingReported,
  Other,
  InspectorEvent'
  #-}

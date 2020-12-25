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
        InspectorEventAssessmentRunStarted,
        InspectorEventAssessmentRunCompleted,
        InspectorEventAssessmentRunStateChanged,
        InspectorEventFindingReported,
        InspectorEventOther,
        fromInspectorEvent
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype InspectorEvent = InspectorEvent'
  { fromInspectorEvent ::
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

pattern InspectorEventAssessmentRunStarted :: InspectorEvent
pattern InspectorEventAssessmentRunStarted = InspectorEvent' "ASSESSMENT_RUN_STARTED"

pattern InspectorEventAssessmentRunCompleted :: InspectorEvent
pattern InspectorEventAssessmentRunCompleted = InspectorEvent' "ASSESSMENT_RUN_COMPLETED"

pattern InspectorEventAssessmentRunStateChanged :: InspectorEvent
pattern InspectorEventAssessmentRunStateChanged = InspectorEvent' "ASSESSMENT_RUN_STATE_CHANGED"

pattern InspectorEventFindingReported :: InspectorEvent
pattern InspectorEventFindingReported = InspectorEvent' "FINDING_REPORTED"

pattern InspectorEventOther :: InspectorEvent
pattern InspectorEventOther = InspectorEvent' "OTHER"

{-# COMPLETE
  InspectorEventAssessmentRunStarted,
  InspectorEventAssessmentRunCompleted,
  InspectorEventAssessmentRunStateChanged,
  InspectorEventFindingReported,
  InspectorEventOther,
  InspectorEvent'
  #-}

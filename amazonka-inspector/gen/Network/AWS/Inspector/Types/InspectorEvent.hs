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
-- Module      : Network.AWS.Inspector.Types.InspectorEvent
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.InspectorEvent
  ( InspectorEvent
      ( ..,
        InspectorEvent_ASSESSMENT_RUN_COMPLETED,
        InspectorEvent_ASSESSMENT_RUN_STARTED,
        InspectorEvent_ASSESSMENT_RUN_STATE_CHANGED,
        InspectorEvent_FINDING_REPORTED,
        InspectorEvent_OTHER
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype InspectorEvent = InspectorEvent'
  { fromInspectorEvent ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern InspectorEvent_ASSESSMENT_RUN_COMPLETED :: InspectorEvent
pattern InspectorEvent_ASSESSMENT_RUN_COMPLETED = InspectorEvent' "ASSESSMENT_RUN_COMPLETED"

pattern InspectorEvent_ASSESSMENT_RUN_STARTED :: InspectorEvent
pattern InspectorEvent_ASSESSMENT_RUN_STARTED = InspectorEvent' "ASSESSMENT_RUN_STARTED"

pattern InspectorEvent_ASSESSMENT_RUN_STATE_CHANGED :: InspectorEvent
pattern InspectorEvent_ASSESSMENT_RUN_STATE_CHANGED = InspectorEvent' "ASSESSMENT_RUN_STATE_CHANGED"

pattern InspectorEvent_FINDING_REPORTED :: InspectorEvent
pattern InspectorEvent_FINDING_REPORTED = InspectorEvent' "FINDING_REPORTED"

pattern InspectorEvent_OTHER :: InspectorEvent
pattern InspectorEvent_OTHER = InspectorEvent' "OTHER"

{-# COMPLETE
  InspectorEvent_ASSESSMENT_RUN_COMPLETED,
  InspectorEvent_ASSESSMENT_RUN_STARTED,
  InspectorEvent_ASSESSMENT_RUN_STATE_CHANGED,
  InspectorEvent_FINDING_REPORTED,
  InspectorEvent_OTHER,
  InspectorEvent'
  #-}

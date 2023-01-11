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
-- Module      : Amazonka.Inspector.Types.InspectorEvent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector.Types.InspectorEvent
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype InspectorEvent = InspectorEvent'
  { fromInspectorEvent ::
      Data.Text
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
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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

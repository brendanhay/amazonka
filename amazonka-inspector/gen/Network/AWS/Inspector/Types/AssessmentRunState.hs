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
-- Module      : Network.AWS.Inspector.Types.AssessmentRunState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.AssessmentRunState
  ( AssessmentRunState
      ( ..,
        AssessmentRunState_CANCELED,
        AssessmentRunState_COLLECTING_DATA,
        AssessmentRunState_COMPLETED,
        AssessmentRunState_COMPLETED_WITH_ERRORS,
        AssessmentRunState_CREATED,
        AssessmentRunState_DATA_COLLECTED,
        AssessmentRunState_ERROR,
        AssessmentRunState_EVALUATING_RULES,
        AssessmentRunState_FAILED,
        AssessmentRunState_START_DATA_COLLECTION_IN_PROGRESS,
        AssessmentRunState_START_DATA_COLLECTION_PENDING,
        AssessmentRunState_START_EVALUATING_RULES_PENDING,
        AssessmentRunState_STOP_DATA_COLLECTION_PENDING
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype AssessmentRunState = AssessmentRunState'
  { fromAssessmentRunState ::
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

pattern AssessmentRunState_CANCELED :: AssessmentRunState
pattern AssessmentRunState_CANCELED = AssessmentRunState' "CANCELED"

pattern AssessmentRunState_COLLECTING_DATA :: AssessmentRunState
pattern AssessmentRunState_COLLECTING_DATA = AssessmentRunState' "COLLECTING_DATA"

pattern AssessmentRunState_COMPLETED :: AssessmentRunState
pattern AssessmentRunState_COMPLETED = AssessmentRunState' "COMPLETED"

pattern AssessmentRunState_COMPLETED_WITH_ERRORS :: AssessmentRunState
pattern AssessmentRunState_COMPLETED_WITH_ERRORS = AssessmentRunState' "COMPLETED_WITH_ERRORS"

pattern AssessmentRunState_CREATED :: AssessmentRunState
pattern AssessmentRunState_CREATED = AssessmentRunState' "CREATED"

pattern AssessmentRunState_DATA_COLLECTED :: AssessmentRunState
pattern AssessmentRunState_DATA_COLLECTED = AssessmentRunState' "DATA_COLLECTED"

pattern AssessmentRunState_ERROR :: AssessmentRunState
pattern AssessmentRunState_ERROR = AssessmentRunState' "ERROR"

pattern AssessmentRunState_EVALUATING_RULES :: AssessmentRunState
pattern AssessmentRunState_EVALUATING_RULES = AssessmentRunState' "EVALUATING_RULES"

pattern AssessmentRunState_FAILED :: AssessmentRunState
pattern AssessmentRunState_FAILED = AssessmentRunState' "FAILED"

pattern AssessmentRunState_START_DATA_COLLECTION_IN_PROGRESS :: AssessmentRunState
pattern AssessmentRunState_START_DATA_COLLECTION_IN_PROGRESS = AssessmentRunState' "START_DATA_COLLECTION_IN_PROGRESS"

pattern AssessmentRunState_START_DATA_COLLECTION_PENDING :: AssessmentRunState
pattern AssessmentRunState_START_DATA_COLLECTION_PENDING = AssessmentRunState' "START_DATA_COLLECTION_PENDING"

pattern AssessmentRunState_START_EVALUATING_RULES_PENDING :: AssessmentRunState
pattern AssessmentRunState_START_EVALUATING_RULES_PENDING = AssessmentRunState' "START_EVALUATING_RULES_PENDING"

pattern AssessmentRunState_STOP_DATA_COLLECTION_PENDING :: AssessmentRunState
pattern AssessmentRunState_STOP_DATA_COLLECTION_PENDING = AssessmentRunState' "STOP_DATA_COLLECTION_PENDING"

{-# COMPLETE
  AssessmentRunState_CANCELED,
  AssessmentRunState_COLLECTING_DATA,
  AssessmentRunState_COMPLETED,
  AssessmentRunState_COMPLETED_WITH_ERRORS,
  AssessmentRunState_CREATED,
  AssessmentRunState_DATA_COLLECTED,
  AssessmentRunState_ERROR,
  AssessmentRunState_EVALUATING_RULES,
  AssessmentRunState_FAILED,
  AssessmentRunState_START_DATA_COLLECTION_IN_PROGRESS,
  AssessmentRunState_START_DATA_COLLECTION_PENDING,
  AssessmentRunState_START_EVALUATING_RULES_PENDING,
  AssessmentRunState_STOP_DATA_COLLECTION_PENDING,
  AssessmentRunState'
  #-}

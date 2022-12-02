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
-- Module      : Amazonka.Inspector.Types.AssessmentRunState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector.Types.AssessmentRunState
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AssessmentRunState = AssessmentRunState'
  { fromAssessmentRunState ::
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

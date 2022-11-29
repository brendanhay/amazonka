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
-- Module      : Amazonka.ComprehendMedical.Types.AttributeName
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComprehendMedical.Types.AttributeName
  ( AttributeName
      ( ..,
        AttributeName_DIAGNOSIS,
        AttributeName_FUTURE,
        AttributeName_HYPOTHETICAL,
        AttributeName_LOW_CONFIDENCE,
        AttributeName_NEGATION,
        AttributeName_PAST_HISTORY,
        AttributeName_PERTAINS_TO_FAMILY,
        AttributeName_SIGN,
        AttributeName_SYMPTOM
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype AttributeName = AttributeName'
  { fromAttributeName ::
      Core.Text
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

pattern AttributeName_DIAGNOSIS :: AttributeName
pattern AttributeName_DIAGNOSIS = AttributeName' "DIAGNOSIS"

pattern AttributeName_FUTURE :: AttributeName
pattern AttributeName_FUTURE = AttributeName' "FUTURE"

pattern AttributeName_HYPOTHETICAL :: AttributeName
pattern AttributeName_HYPOTHETICAL = AttributeName' "HYPOTHETICAL"

pattern AttributeName_LOW_CONFIDENCE :: AttributeName
pattern AttributeName_LOW_CONFIDENCE = AttributeName' "LOW_CONFIDENCE"

pattern AttributeName_NEGATION :: AttributeName
pattern AttributeName_NEGATION = AttributeName' "NEGATION"

pattern AttributeName_PAST_HISTORY :: AttributeName
pattern AttributeName_PAST_HISTORY = AttributeName' "PAST_HISTORY"

pattern AttributeName_PERTAINS_TO_FAMILY :: AttributeName
pattern AttributeName_PERTAINS_TO_FAMILY = AttributeName' "PERTAINS_TO_FAMILY"

pattern AttributeName_SIGN :: AttributeName
pattern AttributeName_SIGN = AttributeName' "SIGN"

pattern AttributeName_SYMPTOM :: AttributeName
pattern AttributeName_SYMPTOM = AttributeName' "SYMPTOM"

{-# COMPLETE
  AttributeName_DIAGNOSIS,
  AttributeName_FUTURE,
  AttributeName_HYPOTHETICAL,
  AttributeName_LOW_CONFIDENCE,
  AttributeName_NEGATION,
  AttributeName_PAST_HISTORY,
  AttributeName_PERTAINS_TO_FAMILY,
  AttributeName_SIGN,
  AttributeName_SYMPTOM,
  AttributeName'
  #-}

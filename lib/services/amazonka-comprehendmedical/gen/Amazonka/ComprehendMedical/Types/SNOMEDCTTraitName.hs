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
-- Module      : Amazonka.ComprehendMedical.Types.SNOMEDCTTraitName
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComprehendMedical.Types.SNOMEDCTTraitName
  ( SNOMEDCTTraitName
      ( ..,
        SNOMEDCTTraitName_DIAGNOSIS,
        SNOMEDCTTraitName_FUTURE,
        SNOMEDCTTraitName_HYPOTHETICAL,
        SNOMEDCTTraitName_LOW_CONFIDENCE,
        SNOMEDCTTraitName_NEGATION,
        SNOMEDCTTraitName_PAST_HISTORY,
        SNOMEDCTTraitName_PERTAINS_TO_FAMILY,
        SNOMEDCTTraitName_SIGN,
        SNOMEDCTTraitName_SYMPTOM
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SNOMEDCTTraitName = SNOMEDCTTraitName'
  { fromSNOMEDCTTraitName ::
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

pattern SNOMEDCTTraitName_DIAGNOSIS :: SNOMEDCTTraitName
pattern SNOMEDCTTraitName_DIAGNOSIS = SNOMEDCTTraitName' "DIAGNOSIS"

pattern SNOMEDCTTraitName_FUTURE :: SNOMEDCTTraitName
pattern SNOMEDCTTraitName_FUTURE = SNOMEDCTTraitName' "FUTURE"

pattern SNOMEDCTTraitName_HYPOTHETICAL :: SNOMEDCTTraitName
pattern SNOMEDCTTraitName_HYPOTHETICAL = SNOMEDCTTraitName' "HYPOTHETICAL"

pattern SNOMEDCTTraitName_LOW_CONFIDENCE :: SNOMEDCTTraitName
pattern SNOMEDCTTraitName_LOW_CONFIDENCE = SNOMEDCTTraitName' "LOW_CONFIDENCE"

pattern SNOMEDCTTraitName_NEGATION :: SNOMEDCTTraitName
pattern SNOMEDCTTraitName_NEGATION = SNOMEDCTTraitName' "NEGATION"

pattern SNOMEDCTTraitName_PAST_HISTORY :: SNOMEDCTTraitName
pattern SNOMEDCTTraitName_PAST_HISTORY = SNOMEDCTTraitName' "PAST_HISTORY"

pattern SNOMEDCTTraitName_PERTAINS_TO_FAMILY :: SNOMEDCTTraitName
pattern SNOMEDCTTraitName_PERTAINS_TO_FAMILY = SNOMEDCTTraitName' "PERTAINS_TO_FAMILY"

pattern SNOMEDCTTraitName_SIGN :: SNOMEDCTTraitName
pattern SNOMEDCTTraitName_SIGN = SNOMEDCTTraitName' "SIGN"

pattern SNOMEDCTTraitName_SYMPTOM :: SNOMEDCTTraitName
pattern SNOMEDCTTraitName_SYMPTOM = SNOMEDCTTraitName' "SYMPTOM"

{-# COMPLETE
  SNOMEDCTTraitName_DIAGNOSIS,
  SNOMEDCTTraitName_FUTURE,
  SNOMEDCTTraitName_HYPOTHETICAL,
  SNOMEDCTTraitName_LOW_CONFIDENCE,
  SNOMEDCTTraitName_NEGATION,
  SNOMEDCTTraitName_PAST_HISTORY,
  SNOMEDCTTraitName_PERTAINS_TO_FAMILY,
  SNOMEDCTTraitName_SIGN,
  SNOMEDCTTraitName_SYMPTOM,
  SNOMEDCTTraitName'
  #-}

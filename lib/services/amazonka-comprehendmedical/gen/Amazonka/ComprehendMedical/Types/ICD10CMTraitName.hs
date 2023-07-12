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
-- Module      : Amazonka.ComprehendMedical.Types.ICD10CMTraitName
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComprehendMedical.Types.ICD10CMTraitName
  ( ICD10CMTraitName
      ( ..,
        ICD10CMTraitName_DIAGNOSIS,
        ICD10CMTraitName_HYPOTHETICAL,
        ICD10CMTraitName_LOW_CONFIDENCE,
        ICD10CMTraitName_NEGATION,
        ICD10CMTraitName_PERTAINS_TO_FAMILY,
        ICD10CMTraitName_SIGN,
        ICD10CMTraitName_SYMPTOM
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ICD10CMTraitName = ICD10CMTraitName'
  { fromICD10CMTraitName ::
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

pattern ICD10CMTraitName_DIAGNOSIS :: ICD10CMTraitName
pattern ICD10CMTraitName_DIAGNOSIS = ICD10CMTraitName' "DIAGNOSIS"

pattern ICD10CMTraitName_HYPOTHETICAL :: ICD10CMTraitName
pattern ICD10CMTraitName_HYPOTHETICAL = ICD10CMTraitName' "HYPOTHETICAL"

pattern ICD10CMTraitName_LOW_CONFIDENCE :: ICD10CMTraitName
pattern ICD10CMTraitName_LOW_CONFIDENCE = ICD10CMTraitName' "LOW_CONFIDENCE"

pattern ICD10CMTraitName_NEGATION :: ICD10CMTraitName
pattern ICD10CMTraitName_NEGATION = ICD10CMTraitName' "NEGATION"

pattern ICD10CMTraitName_PERTAINS_TO_FAMILY :: ICD10CMTraitName
pattern ICD10CMTraitName_PERTAINS_TO_FAMILY = ICD10CMTraitName' "PERTAINS_TO_FAMILY"

pattern ICD10CMTraitName_SIGN :: ICD10CMTraitName
pattern ICD10CMTraitName_SIGN = ICD10CMTraitName' "SIGN"

pattern ICD10CMTraitName_SYMPTOM :: ICD10CMTraitName
pattern ICD10CMTraitName_SYMPTOM = ICD10CMTraitName' "SYMPTOM"

{-# COMPLETE
  ICD10CMTraitName_DIAGNOSIS,
  ICD10CMTraitName_HYPOTHETICAL,
  ICD10CMTraitName_LOW_CONFIDENCE,
  ICD10CMTraitName_NEGATION,
  ICD10CMTraitName_PERTAINS_TO_FAMILY,
  ICD10CMTraitName_SIGN,
  ICD10CMTraitName_SYMPTOM,
  ICD10CMTraitName'
  #-}

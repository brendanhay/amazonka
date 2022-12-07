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
-- Module      : Amazonka.ComprehendMedical.Types.EntityType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComprehendMedical.Types.EntityType
  ( EntityType
      ( ..,
        EntityType_ANATOMY,
        EntityType_BEHAVIORAL_ENVIRONMENTAL_SOCIAL,
        EntityType_MEDICAL_CONDITION,
        EntityType_MEDICATION,
        EntityType_PROTECTED_HEALTH_INFORMATION,
        EntityType_TEST_TREATMENT_PROCEDURE,
        EntityType_TIME_EXPRESSION
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype EntityType = EntityType'
  { fromEntityType ::
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

pattern EntityType_ANATOMY :: EntityType
pattern EntityType_ANATOMY = EntityType' "ANATOMY"

pattern EntityType_BEHAVIORAL_ENVIRONMENTAL_SOCIAL :: EntityType
pattern EntityType_BEHAVIORAL_ENVIRONMENTAL_SOCIAL = EntityType' "BEHAVIORAL_ENVIRONMENTAL_SOCIAL"

pattern EntityType_MEDICAL_CONDITION :: EntityType
pattern EntityType_MEDICAL_CONDITION = EntityType' "MEDICAL_CONDITION"

pattern EntityType_MEDICATION :: EntityType
pattern EntityType_MEDICATION = EntityType' "MEDICATION"

pattern EntityType_PROTECTED_HEALTH_INFORMATION :: EntityType
pattern EntityType_PROTECTED_HEALTH_INFORMATION = EntityType' "PROTECTED_HEALTH_INFORMATION"

pattern EntityType_TEST_TREATMENT_PROCEDURE :: EntityType
pattern EntityType_TEST_TREATMENT_PROCEDURE = EntityType' "TEST_TREATMENT_PROCEDURE"

pattern EntityType_TIME_EXPRESSION :: EntityType
pattern EntityType_TIME_EXPRESSION = EntityType' "TIME_EXPRESSION"

{-# COMPLETE
  EntityType_ANATOMY,
  EntityType_BEHAVIORAL_ENVIRONMENTAL_SOCIAL,
  EntityType_MEDICAL_CONDITION,
  EntityType_MEDICATION,
  EntityType_PROTECTED_HEALTH_INFORMATION,
  EntityType_TEST_TREATMENT_PROCEDURE,
  EntityType_TIME_EXPRESSION,
  EntityType'
  #-}

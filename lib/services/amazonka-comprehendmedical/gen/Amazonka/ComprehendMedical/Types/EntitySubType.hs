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
-- Module      : Amazonka.ComprehendMedical.Types.EntitySubType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComprehendMedical.Types.EntitySubType
  ( EntitySubType
      ( ..,
        EntitySubType_ACUITY,
        EntitySubType_ADDRESS,
        EntitySubType_AGE,
        EntitySubType_BRAND_NAME,
        EntitySubType_CONTACT_POINT,
        EntitySubType_DATE,
        EntitySubType_DIRECTION,
        EntitySubType_DOSAGE,
        EntitySubType_DURATION,
        EntitySubType_EMAIL,
        EntitySubType_FORM,
        EntitySubType_FREQUENCY,
        EntitySubType_GENERIC_NAME,
        EntitySubType_IDENTIFIER,
        EntitySubType_NAME,
        EntitySubType_PROCEDURE_NAME,
        EntitySubType_PROFESSION,
        EntitySubType_QUALITY,
        EntitySubType_QUANTITY,
        EntitySubType_RATE,
        EntitySubType_ROUTE_OR_MODE,
        EntitySubType_STRENGTH,
        EntitySubType_SYSTEM_ORGAN_SITE,
        EntitySubType_TEST_NAME,
        EntitySubType_TEST_UNITS,
        EntitySubType_TEST_VALUE,
        EntitySubType_TIME_EXPRESSION,
        EntitySubType_TIME_TO_DX_NAME,
        EntitySubType_TIME_TO_MEDICATION_NAME,
        EntitySubType_TIME_TO_PROCEDURE_NAME,
        EntitySubType_TIME_TO_TEST_NAME,
        EntitySubType_TIME_TO_TREATMENT_NAME,
        EntitySubType_TREATMENT_NAME,
        EntitySubType_URL
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype EntitySubType = EntitySubType'
  { fromEntitySubType ::
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

pattern EntitySubType_ACUITY :: EntitySubType
pattern EntitySubType_ACUITY = EntitySubType' "ACUITY"

pattern EntitySubType_ADDRESS :: EntitySubType
pattern EntitySubType_ADDRESS = EntitySubType' "ADDRESS"

pattern EntitySubType_AGE :: EntitySubType
pattern EntitySubType_AGE = EntitySubType' "AGE"

pattern EntitySubType_BRAND_NAME :: EntitySubType
pattern EntitySubType_BRAND_NAME = EntitySubType' "BRAND_NAME"

pattern EntitySubType_CONTACT_POINT :: EntitySubType
pattern EntitySubType_CONTACT_POINT = EntitySubType' "CONTACT_POINT"

pattern EntitySubType_DATE :: EntitySubType
pattern EntitySubType_DATE = EntitySubType' "DATE"

pattern EntitySubType_DIRECTION :: EntitySubType
pattern EntitySubType_DIRECTION = EntitySubType' "DIRECTION"

pattern EntitySubType_DOSAGE :: EntitySubType
pattern EntitySubType_DOSAGE = EntitySubType' "DOSAGE"

pattern EntitySubType_DURATION :: EntitySubType
pattern EntitySubType_DURATION = EntitySubType' "DURATION"

pattern EntitySubType_EMAIL :: EntitySubType
pattern EntitySubType_EMAIL = EntitySubType' "EMAIL"

pattern EntitySubType_FORM :: EntitySubType
pattern EntitySubType_FORM = EntitySubType' "FORM"

pattern EntitySubType_FREQUENCY :: EntitySubType
pattern EntitySubType_FREQUENCY = EntitySubType' "FREQUENCY"

pattern EntitySubType_GENERIC_NAME :: EntitySubType
pattern EntitySubType_GENERIC_NAME = EntitySubType' "GENERIC_NAME"

pattern EntitySubType_IDENTIFIER :: EntitySubType
pattern EntitySubType_IDENTIFIER = EntitySubType' "IDENTIFIER"

pattern EntitySubType_NAME :: EntitySubType
pattern EntitySubType_NAME = EntitySubType' "NAME"

pattern EntitySubType_PROCEDURE_NAME :: EntitySubType
pattern EntitySubType_PROCEDURE_NAME = EntitySubType' "PROCEDURE_NAME"

pattern EntitySubType_PROFESSION :: EntitySubType
pattern EntitySubType_PROFESSION = EntitySubType' "PROFESSION"

pattern EntitySubType_QUALITY :: EntitySubType
pattern EntitySubType_QUALITY = EntitySubType' "QUALITY"

pattern EntitySubType_QUANTITY :: EntitySubType
pattern EntitySubType_QUANTITY = EntitySubType' "QUANTITY"

pattern EntitySubType_RATE :: EntitySubType
pattern EntitySubType_RATE = EntitySubType' "RATE"

pattern EntitySubType_ROUTE_OR_MODE :: EntitySubType
pattern EntitySubType_ROUTE_OR_MODE = EntitySubType' "ROUTE_OR_MODE"

pattern EntitySubType_STRENGTH :: EntitySubType
pattern EntitySubType_STRENGTH = EntitySubType' "STRENGTH"

pattern EntitySubType_SYSTEM_ORGAN_SITE :: EntitySubType
pattern EntitySubType_SYSTEM_ORGAN_SITE = EntitySubType' "SYSTEM_ORGAN_SITE"

pattern EntitySubType_TEST_NAME :: EntitySubType
pattern EntitySubType_TEST_NAME = EntitySubType' "TEST_NAME"

pattern EntitySubType_TEST_UNITS :: EntitySubType
pattern EntitySubType_TEST_UNITS = EntitySubType' "TEST_UNITS"

pattern EntitySubType_TEST_VALUE :: EntitySubType
pattern EntitySubType_TEST_VALUE = EntitySubType' "TEST_VALUE"

pattern EntitySubType_TIME_EXPRESSION :: EntitySubType
pattern EntitySubType_TIME_EXPRESSION = EntitySubType' "TIME_EXPRESSION"

pattern EntitySubType_TIME_TO_DX_NAME :: EntitySubType
pattern EntitySubType_TIME_TO_DX_NAME = EntitySubType' "TIME_TO_DX_NAME"

pattern EntitySubType_TIME_TO_MEDICATION_NAME :: EntitySubType
pattern EntitySubType_TIME_TO_MEDICATION_NAME = EntitySubType' "TIME_TO_MEDICATION_NAME"

pattern EntitySubType_TIME_TO_PROCEDURE_NAME :: EntitySubType
pattern EntitySubType_TIME_TO_PROCEDURE_NAME = EntitySubType' "TIME_TO_PROCEDURE_NAME"

pattern EntitySubType_TIME_TO_TEST_NAME :: EntitySubType
pattern EntitySubType_TIME_TO_TEST_NAME = EntitySubType' "TIME_TO_TEST_NAME"

pattern EntitySubType_TIME_TO_TREATMENT_NAME :: EntitySubType
pattern EntitySubType_TIME_TO_TREATMENT_NAME = EntitySubType' "TIME_TO_TREATMENT_NAME"

pattern EntitySubType_TREATMENT_NAME :: EntitySubType
pattern EntitySubType_TREATMENT_NAME = EntitySubType' "TREATMENT_NAME"

pattern EntitySubType_URL :: EntitySubType
pattern EntitySubType_URL = EntitySubType' "URL"

{-# COMPLETE
  EntitySubType_ACUITY,
  EntitySubType_ADDRESS,
  EntitySubType_AGE,
  EntitySubType_BRAND_NAME,
  EntitySubType_CONTACT_POINT,
  EntitySubType_DATE,
  EntitySubType_DIRECTION,
  EntitySubType_DOSAGE,
  EntitySubType_DURATION,
  EntitySubType_EMAIL,
  EntitySubType_FORM,
  EntitySubType_FREQUENCY,
  EntitySubType_GENERIC_NAME,
  EntitySubType_IDENTIFIER,
  EntitySubType_NAME,
  EntitySubType_PROCEDURE_NAME,
  EntitySubType_PROFESSION,
  EntitySubType_QUALITY,
  EntitySubType_QUANTITY,
  EntitySubType_RATE,
  EntitySubType_ROUTE_OR_MODE,
  EntitySubType_STRENGTH,
  EntitySubType_SYSTEM_ORGAN_SITE,
  EntitySubType_TEST_NAME,
  EntitySubType_TEST_UNITS,
  EntitySubType_TEST_VALUE,
  EntitySubType_TIME_EXPRESSION,
  EntitySubType_TIME_TO_DX_NAME,
  EntitySubType_TIME_TO_MEDICATION_NAME,
  EntitySubType_TIME_TO_PROCEDURE_NAME,
  EntitySubType_TIME_TO_TEST_NAME,
  EntitySubType_TIME_TO_TREATMENT_NAME,
  EntitySubType_TREATMENT_NAME,
  EntitySubType_URL,
  EntitySubType'
  #-}

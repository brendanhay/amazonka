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
-- Module      : Amazonka.ComprehendMedical.Types.RelationshipType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComprehendMedical.Types.RelationshipType
  ( RelationshipType
      ( ..,
        RelationshipType_ACUITY,
        RelationshipType_ADMINISTERED_VIA,
        RelationshipType_AMOUNT,
        RelationshipType_DIRECTION,
        RelationshipType_DOSAGE,
        RelationshipType_DURATION,
        RelationshipType_EVERY,
        RelationshipType_FOR,
        RelationshipType_FORM,
        RelationshipType_FREQUENCY,
        RelationshipType_NEGATIVE,
        RelationshipType_OVERLAP,
        RelationshipType_RATE,
        RelationshipType_ROUTE_OR_MODE,
        RelationshipType_STRENGTH,
        RelationshipType_SYSTEM_ORGAN_SITE,
        RelationshipType_TEST_UNIT,
        RelationshipType_TEST_UNITS,
        RelationshipType_TEST_VALUE,
        RelationshipType_WITH_DOSAGE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype RelationshipType = RelationshipType'
  { fromRelationshipType ::
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

pattern RelationshipType_ACUITY :: RelationshipType
pattern RelationshipType_ACUITY = RelationshipType' "ACUITY"

pattern RelationshipType_ADMINISTERED_VIA :: RelationshipType
pattern RelationshipType_ADMINISTERED_VIA = RelationshipType' "ADMINISTERED_VIA"

pattern RelationshipType_AMOUNT :: RelationshipType
pattern RelationshipType_AMOUNT = RelationshipType' "AMOUNT"

pattern RelationshipType_DIRECTION :: RelationshipType
pattern RelationshipType_DIRECTION = RelationshipType' "DIRECTION"

pattern RelationshipType_DOSAGE :: RelationshipType
pattern RelationshipType_DOSAGE = RelationshipType' "DOSAGE"

pattern RelationshipType_DURATION :: RelationshipType
pattern RelationshipType_DURATION = RelationshipType' "DURATION"

pattern RelationshipType_EVERY :: RelationshipType
pattern RelationshipType_EVERY = RelationshipType' "EVERY"

pattern RelationshipType_FOR :: RelationshipType
pattern RelationshipType_FOR = RelationshipType' "FOR"

pattern RelationshipType_FORM :: RelationshipType
pattern RelationshipType_FORM = RelationshipType' "FORM"

pattern RelationshipType_FREQUENCY :: RelationshipType
pattern RelationshipType_FREQUENCY = RelationshipType' "FREQUENCY"

pattern RelationshipType_NEGATIVE :: RelationshipType
pattern RelationshipType_NEGATIVE = RelationshipType' "NEGATIVE"

pattern RelationshipType_OVERLAP :: RelationshipType
pattern RelationshipType_OVERLAP = RelationshipType' "OVERLAP"

pattern RelationshipType_RATE :: RelationshipType
pattern RelationshipType_RATE = RelationshipType' "RATE"

pattern RelationshipType_ROUTE_OR_MODE :: RelationshipType
pattern RelationshipType_ROUTE_OR_MODE = RelationshipType' "ROUTE_OR_MODE"

pattern RelationshipType_STRENGTH :: RelationshipType
pattern RelationshipType_STRENGTH = RelationshipType' "STRENGTH"

pattern RelationshipType_SYSTEM_ORGAN_SITE :: RelationshipType
pattern RelationshipType_SYSTEM_ORGAN_SITE = RelationshipType' "SYSTEM_ORGAN_SITE"

pattern RelationshipType_TEST_UNIT :: RelationshipType
pattern RelationshipType_TEST_UNIT = RelationshipType' "TEST_UNIT"

pattern RelationshipType_TEST_UNITS :: RelationshipType
pattern RelationshipType_TEST_UNITS = RelationshipType' "TEST_UNITS"

pattern RelationshipType_TEST_VALUE :: RelationshipType
pattern RelationshipType_TEST_VALUE = RelationshipType' "TEST_VALUE"

pattern RelationshipType_WITH_DOSAGE :: RelationshipType
pattern RelationshipType_WITH_DOSAGE = RelationshipType' "WITH_DOSAGE"

{-# COMPLETE
  RelationshipType_ACUITY,
  RelationshipType_ADMINISTERED_VIA,
  RelationshipType_AMOUNT,
  RelationshipType_DIRECTION,
  RelationshipType_DOSAGE,
  RelationshipType_DURATION,
  RelationshipType_EVERY,
  RelationshipType_FOR,
  RelationshipType_FORM,
  RelationshipType_FREQUENCY,
  RelationshipType_NEGATIVE,
  RelationshipType_OVERLAP,
  RelationshipType_RATE,
  RelationshipType_ROUTE_OR_MODE,
  RelationshipType_STRENGTH,
  RelationshipType_SYSTEM_ORGAN_SITE,
  RelationshipType_TEST_UNIT,
  RelationshipType_TEST_UNITS,
  RelationshipType_TEST_VALUE,
  RelationshipType_WITH_DOSAGE,
  RelationshipType'
  #-}

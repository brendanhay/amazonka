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
-- Module      : Amazonka.ComprehendMedical.Types.SNOMEDCTRelationshipType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComprehendMedical.Types.SNOMEDCTRelationshipType
  ( SNOMEDCTRelationshipType
      ( ..,
        SNOMEDCTRelationshipType_ACUITY,
        SNOMEDCTRelationshipType_DIRECTION,
        SNOMEDCTRelationshipType_QUALITY,
        SNOMEDCTRelationshipType_SYSTEM_ORGAN_SITE,
        SNOMEDCTRelationshipType_TEST_UNITS,
        SNOMEDCTRelationshipType_TEST_VALUE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SNOMEDCTRelationshipType = SNOMEDCTRelationshipType'
  { fromSNOMEDCTRelationshipType ::
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

pattern SNOMEDCTRelationshipType_ACUITY :: SNOMEDCTRelationshipType
pattern SNOMEDCTRelationshipType_ACUITY = SNOMEDCTRelationshipType' "ACUITY"

pattern SNOMEDCTRelationshipType_DIRECTION :: SNOMEDCTRelationshipType
pattern SNOMEDCTRelationshipType_DIRECTION = SNOMEDCTRelationshipType' "DIRECTION"

pattern SNOMEDCTRelationshipType_QUALITY :: SNOMEDCTRelationshipType
pattern SNOMEDCTRelationshipType_QUALITY = SNOMEDCTRelationshipType' "QUALITY"

pattern SNOMEDCTRelationshipType_SYSTEM_ORGAN_SITE :: SNOMEDCTRelationshipType
pattern SNOMEDCTRelationshipType_SYSTEM_ORGAN_SITE = SNOMEDCTRelationshipType' "SYSTEM_ORGAN_SITE"

pattern SNOMEDCTRelationshipType_TEST_UNITS :: SNOMEDCTRelationshipType
pattern SNOMEDCTRelationshipType_TEST_UNITS = SNOMEDCTRelationshipType' "TEST_UNITS"

pattern SNOMEDCTRelationshipType_TEST_VALUE :: SNOMEDCTRelationshipType
pattern SNOMEDCTRelationshipType_TEST_VALUE = SNOMEDCTRelationshipType' "TEST_VALUE"

{-# COMPLETE
  SNOMEDCTRelationshipType_ACUITY,
  SNOMEDCTRelationshipType_DIRECTION,
  SNOMEDCTRelationshipType_QUALITY,
  SNOMEDCTRelationshipType_SYSTEM_ORGAN_SITE,
  SNOMEDCTRelationshipType_TEST_UNITS,
  SNOMEDCTRelationshipType_TEST_VALUE,
  SNOMEDCTRelationshipType'
  #-}

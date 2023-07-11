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
-- Module      : Amazonka.ComprehendMedical.Types.SNOMEDCTAttributeType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComprehendMedical.Types.SNOMEDCTAttributeType
  ( SNOMEDCTAttributeType
      ( ..,
        SNOMEDCTAttributeType_ACUITY,
        SNOMEDCTAttributeType_DIRECTION,
        SNOMEDCTAttributeType_QUALITY,
        SNOMEDCTAttributeType_SYSTEM_ORGAN_SITE,
        SNOMEDCTAttributeType_TEST_UNIT,
        SNOMEDCTAttributeType_TEST_VALUE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SNOMEDCTAttributeType = SNOMEDCTAttributeType'
  { fromSNOMEDCTAttributeType ::
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

pattern SNOMEDCTAttributeType_ACUITY :: SNOMEDCTAttributeType
pattern SNOMEDCTAttributeType_ACUITY = SNOMEDCTAttributeType' "ACUITY"

pattern SNOMEDCTAttributeType_DIRECTION :: SNOMEDCTAttributeType
pattern SNOMEDCTAttributeType_DIRECTION = SNOMEDCTAttributeType' "DIRECTION"

pattern SNOMEDCTAttributeType_QUALITY :: SNOMEDCTAttributeType
pattern SNOMEDCTAttributeType_QUALITY = SNOMEDCTAttributeType' "QUALITY"

pattern SNOMEDCTAttributeType_SYSTEM_ORGAN_SITE :: SNOMEDCTAttributeType
pattern SNOMEDCTAttributeType_SYSTEM_ORGAN_SITE = SNOMEDCTAttributeType' "SYSTEM_ORGAN_SITE"

pattern SNOMEDCTAttributeType_TEST_UNIT :: SNOMEDCTAttributeType
pattern SNOMEDCTAttributeType_TEST_UNIT = SNOMEDCTAttributeType' "TEST_UNIT"

pattern SNOMEDCTAttributeType_TEST_VALUE :: SNOMEDCTAttributeType
pattern SNOMEDCTAttributeType_TEST_VALUE = SNOMEDCTAttributeType' "TEST_VALUE"

{-# COMPLETE
  SNOMEDCTAttributeType_ACUITY,
  SNOMEDCTAttributeType_DIRECTION,
  SNOMEDCTAttributeType_QUALITY,
  SNOMEDCTAttributeType_SYSTEM_ORGAN_SITE,
  SNOMEDCTAttributeType_TEST_UNIT,
  SNOMEDCTAttributeType_TEST_VALUE,
  SNOMEDCTAttributeType'
  #-}

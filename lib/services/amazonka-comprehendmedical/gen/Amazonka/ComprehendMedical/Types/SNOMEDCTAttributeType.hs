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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
import qualified Amazonka.Prelude as Prelude

newtype SNOMEDCTAttributeType = SNOMEDCTAttributeType'
  { fromSNOMEDCTAttributeType ::
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

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
-- Module      : Amazonka.ComprehendMedical.Types.RxNormAttributeType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComprehendMedical.Types.RxNormAttributeType
  ( RxNormAttributeType
      ( ..,
        RxNormAttributeType_DOSAGE,
        RxNormAttributeType_DURATION,
        RxNormAttributeType_FORM,
        RxNormAttributeType_FREQUENCY,
        RxNormAttributeType_RATE,
        RxNormAttributeType_ROUTE_OR_MODE,
        RxNormAttributeType_STRENGTH
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype RxNormAttributeType = RxNormAttributeType'
  { fromRxNormAttributeType ::
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

pattern RxNormAttributeType_DOSAGE :: RxNormAttributeType
pattern RxNormAttributeType_DOSAGE = RxNormAttributeType' "DOSAGE"

pattern RxNormAttributeType_DURATION :: RxNormAttributeType
pattern RxNormAttributeType_DURATION = RxNormAttributeType' "DURATION"

pattern RxNormAttributeType_FORM :: RxNormAttributeType
pattern RxNormAttributeType_FORM = RxNormAttributeType' "FORM"

pattern RxNormAttributeType_FREQUENCY :: RxNormAttributeType
pattern RxNormAttributeType_FREQUENCY = RxNormAttributeType' "FREQUENCY"

pattern RxNormAttributeType_RATE :: RxNormAttributeType
pattern RxNormAttributeType_RATE = RxNormAttributeType' "RATE"

pattern RxNormAttributeType_ROUTE_OR_MODE :: RxNormAttributeType
pattern RxNormAttributeType_ROUTE_OR_MODE = RxNormAttributeType' "ROUTE_OR_MODE"

pattern RxNormAttributeType_STRENGTH :: RxNormAttributeType
pattern RxNormAttributeType_STRENGTH = RxNormAttributeType' "STRENGTH"

{-# COMPLETE
  RxNormAttributeType_DOSAGE,
  RxNormAttributeType_DURATION,
  RxNormAttributeType_FORM,
  RxNormAttributeType_FREQUENCY,
  RxNormAttributeType_RATE,
  RxNormAttributeType_ROUTE_OR_MODE,
  RxNormAttributeType_STRENGTH,
  RxNormAttributeType'
  #-}

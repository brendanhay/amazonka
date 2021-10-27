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
-- Module      : Network.AWS.ComprehendMedical.Types.RxNormAttributeType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ComprehendMedical.Types.RxNormAttributeType
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype RxNormAttributeType = RxNormAttributeType'
  { fromRxNormAttributeType ::
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

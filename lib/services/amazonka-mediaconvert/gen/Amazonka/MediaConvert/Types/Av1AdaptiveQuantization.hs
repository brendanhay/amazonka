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
-- Module      : Amazonka.MediaConvert.Types.Av1AdaptiveQuantization
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.Av1AdaptiveQuantization
  ( Av1AdaptiveQuantization
      ( ..,
        Av1AdaptiveQuantization_HIGH,
        Av1AdaptiveQuantization_HIGHER,
        Av1AdaptiveQuantization_LOW,
        Av1AdaptiveQuantization_MAX,
        Av1AdaptiveQuantization_MEDIUM,
        Av1AdaptiveQuantization_OFF
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specify the strength of any adaptive quantization filters that you
-- enable. The value that you choose here applies to Spatial adaptive
-- quantization (spatialAdaptiveQuantization).
newtype Av1AdaptiveQuantization = Av1AdaptiveQuantization'
  { fromAv1AdaptiveQuantization ::
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

pattern Av1AdaptiveQuantization_HIGH :: Av1AdaptiveQuantization
pattern Av1AdaptiveQuantization_HIGH = Av1AdaptiveQuantization' "HIGH"

pattern Av1AdaptiveQuantization_HIGHER :: Av1AdaptiveQuantization
pattern Av1AdaptiveQuantization_HIGHER = Av1AdaptiveQuantization' "HIGHER"

pattern Av1AdaptiveQuantization_LOW :: Av1AdaptiveQuantization
pattern Av1AdaptiveQuantization_LOW = Av1AdaptiveQuantization' "LOW"

pattern Av1AdaptiveQuantization_MAX :: Av1AdaptiveQuantization
pattern Av1AdaptiveQuantization_MAX = Av1AdaptiveQuantization' "MAX"

pattern Av1AdaptiveQuantization_MEDIUM :: Av1AdaptiveQuantization
pattern Av1AdaptiveQuantization_MEDIUM = Av1AdaptiveQuantization' "MEDIUM"

pattern Av1AdaptiveQuantization_OFF :: Av1AdaptiveQuantization
pattern Av1AdaptiveQuantization_OFF = Av1AdaptiveQuantization' "OFF"

{-# COMPLETE
  Av1AdaptiveQuantization_HIGH,
  Av1AdaptiveQuantization_HIGHER,
  Av1AdaptiveQuantization_LOW,
  Av1AdaptiveQuantization_MAX,
  Av1AdaptiveQuantization_MEDIUM,
  Av1AdaptiveQuantization_OFF,
  Av1AdaptiveQuantization'
  #-}

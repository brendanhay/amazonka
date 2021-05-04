{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Av1AdaptiveQuantization
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Av1AdaptiveQuantization
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

import qualified Network.AWS.Prelude as Prelude

-- | Specify the strength of any adaptive quantization filters that you
-- enable. The value that you choose here applies to Spatial adaptive
-- quantization (spatialAdaptiveQuantization).
newtype Av1AdaptiveQuantization = Av1AdaptiveQuantization'
  { fromAv1AdaptiveQuantization ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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

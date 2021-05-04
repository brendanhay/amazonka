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
-- Module      : Network.AWS.MediaConvert.Types.H265AdaptiveQuantization
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.H265AdaptiveQuantization
  ( H265AdaptiveQuantization
      ( ..,
        H265AdaptiveQuantization_HIGH,
        H265AdaptiveQuantization_HIGHER,
        H265AdaptiveQuantization_LOW,
        H265AdaptiveQuantization_MAX,
        H265AdaptiveQuantization_MEDIUM,
        H265AdaptiveQuantization_OFF
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | Specify the strength of any adaptive quantization filters that you
-- enable. The value that you choose here applies to the following
-- settings: Flicker adaptive quantization (flickerAdaptiveQuantization),
-- Spatial adaptive quantization (spatialAdaptiveQuantization), and
-- Temporal adaptive quantization (temporalAdaptiveQuantization).
newtype H265AdaptiveQuantization = H265AdaptiveQuantization'
  { fromH265AdaptiveQuantization ::
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

pattern H265AdaptiveQuantization_HIGH :: H265AdaptiveQuantization
pattern H265AdaptiveQuantization_HIGH = H265AdaptiveQuantization' "HIGH"

pattern H265AdaptiveQuantization_HIGHER :: H265AdaptiveQuantization
pattern H265AdaptiveQuantization_HIGHER = H265AdaptiveQuantization' "HIGHER"

pattern H265AdaptiveQuantization_LOW :: H265AdaptiveQuantization
pattern H265AdaptiveQuantization_LOW = H265AdaptiveQuantization' "LOW"

pattern H265AdaptiveQuantization_MAX :: H265AdaptiveQuantization
pattern H265AdaptiveQuantization_MAX = H265AdaptiveQuantization' "MAX"

pattern H265AdaptiveQuantization_MEDIUM :: H265AdaptiveQuantization
pattern H265AdaptiveQuantization_MEDIUM = H265AdaptiveQuantization' "MEDIUM"

pattern H265AdaptiveQuantization_OFF :: H265AdaptiveQuantization
pattern H265AdaptiveQuantization_OFF = H265AdaptiveQuantization' "OFF"

{-# COMPLETE
  H265AdaptiveQuantization_HIGH,
  H265AdaptiveQuantization_HIGHER,
  H265AdaptiveQuantization_LOW,
  H265AdaptiveQuantization_MAX,
  H265AdaptiveQuantization_MEDIUM,
  H265AdaptiveQuantization_OFF,
  H265AdaptiveQuantization'
  #-}

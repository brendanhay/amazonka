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
-- Module      : Amazonka.MediaLive.Types.H265AdaptiveQuantization
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.H265AdaptiveQuantization
  ( H265AdaptiveQuantization
      ( ..,
        H265AdaptiveQuantization_AUTO,
        H265AdaptiveQuantization_HIGH,
        H265AdaptiveQuantization_HIGHER,
        H265AdaptiveQuantization_LOW,
        H265AdaptiveQuantization_MAX,
        H265AdaptiveQuantization_MEDIUM,
        H265AdaptiveQuantization_OFF
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | H265 Adaptive Quantization
newtype H265AdaptiveQuantization = H265AdaptiveQuantization'
  { fromH265AdaptiveQuantization ::
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

pattern H265AdaptiveQuantization_AUTO :: H265AdaptiveQuantization
pattern H265AdaptiveQuantization_AUTO = H265AdaptiveQuantization' "AUTO"

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
  H265AdaptiveQuantization_AUTO,
  H265AdaptiveQuantization_HIGH,
  H265AdaptiveQuantization_HIGHER,
  H265AdaptiveQuantization_LOW,
  H265AdaptiveQuantization_MAX,
  H265AdaptiveQuantization_MEDIUM,
  H265AdaptiveQuantization_OFF,
  H265AdaptiveQuantization'
  #-}

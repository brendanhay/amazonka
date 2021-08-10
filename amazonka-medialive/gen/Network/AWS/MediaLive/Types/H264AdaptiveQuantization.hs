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
-- Module      : Network.AWS.MediaLive.Types.H264AdaptiveQuantization
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.H264AdaptiveQuantization
  ( H264AdaptiveQuantization
      ( ..,
        H264AdaptiveQuantization_HIGH,
        H264AdaptiveQuantization_HIGHER,
        H264AdaptiveQuantization_LOW,
        H264AdaptiveQuantization_MAX,
        H264AdaptiveQuantization_MEDIUM,
        H264AdaptiveQuantization_OFF
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | H264 Adaptive Quantization
newtype H264AdaptiveQuantization = H264AdaptiveQuantization'
  { fromH264AdaptiveQuantization ::
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

pattern H264AdaptiveQuantization_HIGH :: H264AdaptiveQuantization
pattern H264AdaptiveQuantization_HIGH = H264AdaptiveQuantization' "HIGH"

pattern H264AdaptiveQuantization_HIGHER :: H264AdaptiveQuantization
pattern H264AdaptiveQuantization_HIGHER = H264AdaptiveQuantization' "HIGHER"

pattern H264AdaptiveQuantization_LOW :: H264AdaptiveQuantization
pattern H264AdaptiveQuantization_LOW = H264AdaptiveQuantization' "LOW"

pattern H264AdaptiveQuantization_MAX :: H264AdaptiveQuantization
pattern H264AdaptiveQuantization_MAX = H264AdaptiveQuantization' "MAX"

pattern H264AdaptiveQuantization_MEDIUM :: H264AdaptiveQuantization
pattern H264AdaptiveQuantization_MEDIUM = H264AdaptiveQuantization' "MEDIUM"

pattern H264AdaptiveQuantization_OFF :: H264AdaptiveQuantization
pattern H264AdaptiveQuantization_OFF = H264AdaptiveQuantization' "OFF"

{-# COMPLETE
  H264AdaptiveQuantization_HIGH,
  H264AdaptiveQuantization_HIGHER,
  H264AdaptiveQuantization_LOW,
  H264AdaptiveQuantization_MAX,
  H264AdaptiveQuantization_MEDIUM,
  H264AdaptiveQuantization_OFF,
  H264AdaptiveQuantization'
  #-}

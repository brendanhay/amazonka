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
-- Module      : Network.AWS.MediaLive.Types.Mpeg2AdaptiveQuantization
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Mpeg2AdaptiveQuantization
  ( Mpeg2AdaptiveQuantization
      ( ..,
        Mpeg2AdaptiveQuantization_AUTO,
        Mpeg2AdaptiveQuantization_HIGH,
        Mpeg2AdaptiveQuantization_LOW,
        Mpeg2AdaptiveQuantization_MEDIUM,
        Mpeg2AdaptiveQuantization_OFF
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | Mpeg2 Adaptive Quantization
newtype Mpeg2AdaptiveQuantization = Mpeg2AdaptiveQuantization'
  { fromMpeg2AdaptiveQuantization ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern Mpeg2AdaptiveQuantization_AUTO :: Mpeg2AdaptiveQuantization
pattern Mpeg2AdaptiveQuantization_AUTO = Mpeg2AdaptiveQuantization' "AUTO"

pattern Mpeg2AdaptiveQuantization_HIGH :: Mpeg2AdaptiveQuantization
pattern Mpeg2AdaptiveQuantization_HIGH = Mpeg2AdaptiveQuantization' "HIGH"

pattern Mpeg2AdaptiveQuantization_LOW :: Mpeg2AdaptiveQuantization
pattern Mpeg2AdaptiveQuantization_LOW = Mpeg2AdaptiveQuantization' "LOW"

pattern Mpeg2AdaptiveQuantization_MEDIUM :: Mpeg2AdaptiveQuantization
pattern Mpeg2AdaptiveQuantization_MEDIUM = Mpeg2AdaptiveQuantization' "MEDIUM"

pattern Mpeg2AdaptiveQuantization_OFF :: Mpeg2AdaptiveQuantization
pattern Mpeg2AdaptiveQuantization_OFF = Mpeg2AdaptiveQuantization' "OFF"

{-# COMPLETE
  Mpeg2AdaptiveQuantization_AUTO,
  Mpeg2AdaptiveQuantization_HIGH,
  Mpeg2AdaptiveQuantization_LOW,
  Mpeg2AdaptiveQuantization_MEDIUM,
  Mpeg2AdaptiveQuantization_OFF,
  Mpeg2AdaptiveQuantization'
  #-}

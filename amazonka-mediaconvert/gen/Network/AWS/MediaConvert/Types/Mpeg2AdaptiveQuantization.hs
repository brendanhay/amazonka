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
-- Module      : Network.AWS.MediaConvert.Types.Mpeg2AdaptiveQuantization
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Mpeg2AdaptiveQuantization
  ( Mpeg2AdaptiveQuantization
      ( ..,
        Mpeg2AdaptiveQuantization_HIGH,
        Mpeg2AdaptiveQuantization_LOW,
        Mpeg2AdaptiveQuantization_MEDIUM,
        Mpeg2AdaptiveQuantization_OFF
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | Specify the strength of any adaptive quantization filters that you
-- enable. The value that you choose here applies to the following
-- settings: Spatial adaptive quantization (spatialAdaptiveQuantization),
-- and Temporal adaptive quantization (temporalAdaptiveQuantization).
newtype Mpeg2AdaptiveQuantization = Mpeg2AdaptiveQuantization'
  { fromMpeg2AdaptiveQuantization ::
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

pattern Mpeg2AdaptiveQuantization_HIGH :: Mpeg2AdaptiveQuantization
pattern Mpeg2AdaptiveQuantization_HIGH = Mpeg2AdaptiveQuantization' "HIGH"

pattern Mpeg2AdaptiveQuantization_LOW :: Mpeg2AdaptiveQuantization
pattern Mpeg2AdaptiveQuantization_LOW = Mpeg2AdaptiveQuantization' "LOW"

pattern Mpeg2AdaptiveQuantization_MEDIUM :: Mpeg2AdaptiveQuantization
pattern Mpeg2AdaptiveQuantization_MEDIUM = Mpeg2AdaptiveQuantization' "MEDIUM"

pattern Mpeg2AdaptiveQuantization_OFF :: Mpeg2AdaptiveQuantization
pattern Mpeg2AdaptiveQuantization_OFF = Mpeg2AdaptiveQuantization' "OFF"

{-# COMPLETE
  Mpeg2AdaptiveQuantization_HIGH,
  Mpeg2AdaptiveQuantization_LOW,
  Mpeg2AdaptiveQuantization_MEDIUM,
  Mpeg2AdaptiveQuantization_OFF,
  Mpeg2AdaptiveQuantization'
  #-}

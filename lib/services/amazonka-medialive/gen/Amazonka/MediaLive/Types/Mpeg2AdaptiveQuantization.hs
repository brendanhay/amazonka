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
-- Module      : Amazonka.MediaLive.Types.Mpeg2AdaptiveQuantization
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.Mpeg2AdaptiveQuantization
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Mpeg2 Adaptive Quantization
newtype Mpeg2AdaptiveQuantization = Mpeg2AdaptiveQuantization'
  { fromMpeg2AdaptiveQuantization ::
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

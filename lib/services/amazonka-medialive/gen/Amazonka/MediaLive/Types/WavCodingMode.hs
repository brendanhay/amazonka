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
-- Module      : Amazonka.MediaLive.Types.WavCodingMode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.WavCodingMode
  ( WavCodingMode
      ( ..,
        WavCodingMode_CODING_MODE_1_0,
        WavCodingMode_CODING_MODE_2_0,
        WavCodingMode_CODING_MODE_4_0,
        WavCodingMode_CODING_MODE_8_0
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

-- | Wav Coding Mode
newtype WavCodingMode = WavCodingMode'
  { fromWavCodingMode ::
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

pattern WavCodingMode_CODING_MODE_1_0 :: WavCodingMode
pattern WavCodingMode_CODING_MODE_1_0 = WavCodingMode' "CODING_MODE_1_0"

pattern WavCodingMode_CODING_MODE_2_0 :: WavCodingMode
pattern WavCodingMode_CODING_MODE_2_0 = WavCodingMode' "CODING_MODE_2_0"

pattern WavCodingMode_CODING_MODE_4_0 :: WavCodingMode
pattern WavCodingMode_CODING_MODE_4_0 = WavCodingMode' "CODING_MODE_4_0"

pattern WavCodingMode_CODING_MODE_8_0 :: WavCodingMode
pattern WavCodingMode_CODING_MODE_8_0 = WavCodingMode' "CODING_MODE_8_0"

{-# COMPLETE
  WavCodingMode_CODING_MODE_1_0,
  WavCodingMode_CODING_MODE_2_0,
  WavCodingMode_CODING_MODE_4_0,
  WavCodingMode_CODING_MODE_8_0,
  WavCodingMode'
  #-}

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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Wav Coding Mode
newtype WavCodingMode = WavCodingMode'
  { fromWavCodingMode ::
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

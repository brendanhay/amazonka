{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.WavCodingMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.WavCodingMode
  ( WavCodingMode
      ( WavCodingMode',
        WavCodingModeCodingMode10,
        WavCodingModeCodingMode20,
        WavCodingModeCodingMode40,
        WavCodingModeCodingMode80,
        fromWavCodingMode
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Wav Coding Mode
newtype WavCodingMode = WavCodingMode'
  { fromWavCodingMode ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern WavCodingModeCodingMode10 :: WavCodingMode
pattern WavCodingModeCodingMode10 = WavCodingMode' "CODING_MODE_1_0"

pattern WavCodingModeCodingMode20 :: WavCodingMode
pattern WavCodingModeCodingMode20 = WavCodingMode' "CODING_MODE_2_0"

pattern WavCodingModeCodingMode40 :: WavCodingMode
pattern WavCodingModeCodingMode40 = WavCodingMode' "CODING_MODE_4_0"

pattern WavCodingModeCodingMode80 :: WavCodingMode
pattern WavCodingModeCodingMode80 = WavCodingMode' "CODING_MODE_8_0"

{-# COMPLETE
  WavCodingModeCodingMode10,
  WavCodingModeCodingMode20,
  WavCodingModeCodingMode40,
  WavCodingModeCodingMode80,
  WavCodingMode'
  #-}

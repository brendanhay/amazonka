{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Eac3AtmosStereoDownmix
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Eac3AtmosStereoDownmix
  ( Eac3AtmosStereoDownmix
      ( Eac3AtmosStereoDownmix',
        Eac3AtmosStereoDownmixNotIndicated,
        Eac3AtmosStereoDownmixStereo,
        Eac3AtmosStereoDownmixSurround,
        Eac3AtmosStereoDownmixDPL2,
        fromEac3AtmosStereoDownmix
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Choose how the service does stereo downmixing.
newtype Eac3AtmosStereoDownmix = Eac3AtmosStereoDownmix'
  { fromEac3AtmosStereoDownmix ::
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

pattern Eac3AtmosStereoDownmixNotIndicated :: Eac3AtmosStereoDownmix
pattern Eac3AtmosStereoDownmixNotIndicated = Eac3AtmosStereoDownmix' "NOT_INDICATED"

pattern Eac3AtmosStereoDownmixStereo :: Eac3AtmosStereoDownmix
pattern Eac3AtmosStereoDownmixStereo = Eac3AtmosStereoDownmix' "STEREO"

pattern Eac3AtmosStereoDownmixSurround :: Eac3AtmosStereoDownmix
pattern Eac3AtmosStereoDownmixSurround = Eac3AtmosStereoDownmix' "SURROUND"

pattern Eac3AtmosStereoDownmixDPL2 :: Eac3AtmosStereoDownmix
pattern Eac3AtmosStereoDownmixDPL2 = Eac3AtmosStereoDownmix' "DPL2"

{-# COMPLETE
  Eac3AtmosStereoDownmixNotIndicated,
  Eac3AtmosStereoDownmixStereo,
  Eac3AtmosStereoDownmixSurround,
  Eac3AtmosStereoDownmixDPL2,
  Eac3AtmosStereoDownmix'
  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Eac3StereoDownmix
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Eac3StereoDownmix
  ( Eac3StereoDownmix
      ( Eac3StereoDownmix',
        Eac3StereoDownmixNotIndicated,
        Eac3StereoDownmixLoRo,
        Eac3StereoDownmixLtRt,
        Eac3StereoDownmixDPL2,
        fromEac3StereoDownmix
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Choose how the service does stereo downmixing. This setting only applies if you keep the default value of 3/2 - L, R, C, Ls, Rs (CODING_MODE_3_2) for the setting Coding mode (Eac3CodingMode). If you choose a different value for Coding mode, the service ignores Stereo downmix (Eac3StereoDownmix).
newtype Eac3StereoDownmix = Eac3StereoDownmix'
  { fromEac3StereoDownmix ::
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

pattern Eac3StereoDownmixNotIndicated :: Eac3StereoDownmix
pattern Eac3StereoDownmixNotIndicated = Eac3StereoDownmix' "NOT_INDICATED"

pattern Eac3StereoDownmixLoRo :: Eac3StereoDownmix
pattern Eac3StereoDownmixLoRo = Eac3StereoDownmix' "LO_RO"

pattern Eac3StereoDownmixLtRt :: Eac3StereoDownmix
pattern Eac3StereoDownmixLtRt = Eac3StereoDownmix' "LT_RT"

pattern Eac3StereoDownmixDPL2 :: Eac3StereoDownmix
pattern Eac3StereoDownmixDPL2 = Eac3StereoDownmix' "DPL2"

{-# COMPLETE
  Eac3StereoDownmixNotIndicated,
  Eac3StereoDownmixLoRo,
  Eac3StereoDownmixLtRt,
  Eac3StereoDownmixDPL2,
  Eac3StereoDownmix'
  #-}

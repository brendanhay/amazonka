{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Eac3StereoDownmix
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.Eac3StereoDownmix
  ( Eac3StereoDownmix
    ( Eac3StereoDownmix'
    , Eac3StereoDownmixDPL2
    , Eac3StereoDownmixLoRo
    , Eac3StereoDownmixLtRt
    , Eac3StereoDownmixNotIndicated
    , fromEac3StereoDownmix
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Eac3 Stereo Downmix
newtype Eac3StereoDownmix = Eac3StereoDownmix'{fromEac3StereoDownmix
                                               :: Core.Text}
                              deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                              Core.Generic)
                              deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                Core.FromJSON, Core.ToXML, Core.FromXML,
                                                Core.ToText, Core.FromText, Core.ToByteString,
                                                Core.ToQuery, Core.ToHeader)

pattern Eac3StereoDownmixDPL2 :: Eac3StereoDownmix
pattern Eac3StereoDownmixDPL2 = Eac3StereoDownmix' "DPL2"

pattern Eac3StereoDownmixLoRo :: Eac3StereoDownmix
pattern Eac3StereoDownmixLoRo = Eac3StereoDownmix' "LO_RO"

pattern Eac3StereoDownmixLtRt :: Eac3StereoDownmix
pattern Eac3StereoDownmixLtRt = Eac3StereoDownmix' "LT_RT"

pattern Eac3StereoDownmixNotIndicated :: Eac3StereoDownmix
pattern Eac3StereoDownmixNotIndicated = Eac3StereoDownmix' "NOT_INDICATED"

{-# COMPLETE 
  Eac3StereoDownmixDPL2,

  Eac3StereoDownmixLoRo,

  Eac3StereoDownmixLtRt,

  Eac3StereoDownmixNotIndicated,
  Eac3StereoDownmix'
  #-}

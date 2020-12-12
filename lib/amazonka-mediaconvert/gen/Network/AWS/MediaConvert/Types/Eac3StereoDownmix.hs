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
        ESDDPL2,
        ESDLoRo,
        ESDLtRt,
        ESDNotIndicated
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Choose how the service does stereo downmixing. This setting only applies if you keep the default value of 3/2 - L, R, C, Ls, Rs (CODING_MODE_3_2) for the setting Coding mode (Eac3CodingMode). If you choose a different value for Coding mode, the service ignores Stereo downmix (Eac3StereoDownmix).
newtype Eac3StereoDownmix = Eac3StereoDownmix' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern ESDDPL2 :: Eac3StereoDownmix
pattern ESDDPL2 = Eac3StereoDownmix' "DPL2"

pattern ESDLoRo :: Eac3StereoDownmix
pattern ESDLoRo = Eac3StereoDownmix' "LO_RO"

pattern ESDLtRt :: Eac3StereoDownmix
pattern ESDLtRt = Eac3StereoDownmix' "LT_RT"

pattern ESDNotIndicated :: Eac3StereoDownmix
pattern ESDNotIndicated = Eac3StereoDownmix' "NOT_INDICATED"

{-# COMPLETE
  ESDDPL2,
  ESDLoRo,
  ESDLtRt,
  ESDNotIndicated,
  Eac3StereoDownmix'
  #-}

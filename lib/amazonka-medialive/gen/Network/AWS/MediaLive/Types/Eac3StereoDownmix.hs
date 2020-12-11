-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Eac3StereoDownmix
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Eac3StereoDownmix
  ( Eac3StereoDownmix
      ( Eac3StereoDownmix',
        DPL2,
        LoRo,
        LtRt,
        NotIndicated
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Eac3 Stereo Downmix
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

pattern DPL2 :: Eac3StereoDownmix
pattern DPL2 = Eac3StereoDownmix' "DPL2"

pattern LoRo :: Eac3StereoDownmix
pattern LoRo = Eac3StereoDownmix' "LO_RO"

pattern LtRt :: Eac3StereoDownmix
pattern LtRt = Eac3StereoDownmix' "LT_RT"

pattern NotIndicated :: Eac3StereoDownmix
pattern NotIndicated = Eac3StereoDownmix' "NOT_INDICATED"

{-# COMPLETE
  DPL2,
  LoRo,
  LtRt,
  NotIndicated,
  Eac3StereoDownmix'
  #-}

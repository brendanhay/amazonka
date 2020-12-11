-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H265CodecProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.H265CodecProfile
  ( H265CodecProfile
      ( H265CodecProfile',
        MAIN10High,
        MAIN10Main,
        Main42210BITHigh,
        Main42210BITMain,
        Main4228BITHigh,
        Main4228BITMain,
        MainHigh,
        MainMain
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Represents the Profile and Tier, per the HEVC (H.265) specification. Selections are grouped as [Profile] / [Tier], so "Main/High" represents Main Profile with High Tier. 4:2:2 profiles are only available with the HEVC 4:2:2 License.
newtype H265CodecProfile = H265CodecProfile' Lude.Text
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

pattern MAIN10High :: H265CodecProfile
pattern MAIN10High = H265CodecProfile' "MAIN10_HIGH"

pattern MAIN10Main :: H265CodecProfile
pattern MAIN10Main = H265CodecProfile' "MAIN10_MAIN"

pattern Main42210BITHigh :: H265CodecProfile
pattern Main42210BITHigh = H265CodecProfile' "MAIN_422_10BIT_HIGH"

pattern Main42210BITMain :: H265CodecProfile
pattern Main42210BITMain = H265CodecProfile' "MAIN_422_10BIT_MAIN"

pattern Main4228BITHigh :: H265CodecProfile
pattern Main4228BITHigh = H265CodecProfile' "MAIN_422_8BIT_HIGH"

pattern Main4228BITMain :: H265CodecProfile
pattern Main4228BITMain = H265CodecProfile' "MAIN_422_8BIT_MAIN"

pattern MainHigh :: H265CodecProfile
pattern MainHigh = H265CodecProfile' "MAIN_HIGH"

pattern MainMain :: H265CodecProfile
pattern MainMain = H265CodecProfile' "MAIN_MAIN"

{-# COMPLETE
  MAIN10High,
  MAIN10Main,
  Main42210BITHigh,
  Main42210BITMain,
  Main4228BITHigh,
  Main4228BITMain,
  MainHigh,
  MainMain,
  H265CodecProfile'
  #-}

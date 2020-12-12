{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H264CodecProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.H264CodecProfile
  ( H264CodecProfile
      ( H264CodecProfile',
        HCPBaseline,
        HCPHigh,
        HCPHigh10BIT,
        HCPHigh422,
        HCPHigh42210BIT,
        HCPMain
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | H.264 Profile. High 4:2:2 and 10-bit profiles are only available with the AVC-I License.
newtype H264CodecProfile = H264CodecProfile' Lude.Text
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

pattern HCPBaseline :: H264CodecProfile
pattern HCPBaseline = H264CodecProfile' "BASELINE"

pattern HCPHigh :: H264CodecProfile
pattern HCPHigh = H264CodecProfile' "HIGH"

pattern HCPHigh10BIT :: H264CodecProfile
pattern HCPHigh10BIT = H264CodecProfile' "HIGH_10BIT"

pattern HCPHigh422 :: H264CodecProfile
pattern HCPHigh422 = H264CodecProfile' "HIGH_422"

pattern HCPHigh42210BIT :: H264CodecProfile
pattern HCPHigh42210BIT = H264CodecProfile' "HIGH_422_10BIT"

pattern HCPMain :: H264CodecProfile
pattern HCPMain = H264CodecProfile' "MAIN"

{-# COMPLETE
  HCPBaseline,
  HCPHigh,
  HCPHigh10BIT,
  HCPHigh422,
  HCPHigh42210BIT,
  HCPMain,
  H264CodecProfile'
  #-}

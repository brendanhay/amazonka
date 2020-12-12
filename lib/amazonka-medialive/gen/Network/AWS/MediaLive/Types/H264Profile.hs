{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H264Profile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.H264Profile
  ( H264Profile
      ( H264Profile',
        HPBaseline,
        HPHigh,
        HPHigh10BIT,
        HPHigh422,
        HPHigh42210BIT,
        HPMain
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | H264 Profile
newtype H264Profile = H264Profile' Lude.Text
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

pattern HPBaseline :: H264Profile
pattern HPBaseline = H264Profile' "BASELINE"

pattern HPHigh :: H264Profile
pattern HPHigh = H264Profile' "HIGH"

pattern HPHigh10BIT :: H264Profile
pattern HPHigh10BIT = H264Profile' "HIGH_10BIT"

pattern HPHigh422 :: H264Profile
pattern HPHigh422 = H264Profile' "HIGH_422"

pattern HPHigh42210BIT :: H264Profile
pattern HPHigh42210BIT = H264Profile' "HIGH_422_10BIT"

pattern HPMain :: H264Profile
pattern HPMain = H264Profile' "MAIN"

{-# COMPLETE
  HPBaseline,
  HPHigh,
  HPHigh10BIT,
  HPHigh422,
  HPHigh42210BIT,
  HPMain,
  H264Profile'
  #-}

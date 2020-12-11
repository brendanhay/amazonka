-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Vp8RateControlMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Vp8RateControlMode
  ( Vp8RateControlMode
      ( Vp8RateControlMode',
        VRCMVbr
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | With the VP8 codec, you can use only the variable bitrate (VBR) rate control mode.
newtype Vp8RateControlMode = Vp8RateControlMode' Lude.Text
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

pattern VRCMVbr :: Vp8RateControlMode
pattern VRCMVbr = Vp8RateControlMode' "VBR"

{-# COMPLETE
  VRCMVbr,
  Vp8RateControlMode'
  #-}

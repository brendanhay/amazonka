{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.MovMpeg2FourCCControl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.MovMpeg2FourCCControl
  ( MovMpeg2FourCCControl
      ( MovMpeg2FourCCControl',
        Xdcam,
        Mpeg
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | When set to XDCAM, writes MPEG2 video streams into the QuickTime file using XDCAM fourcc codes. This increases compatibility with Apple editors and players, but may decrease compatibility with other players. Only applicable when the video codec is MPEG2.
newtype MovMpeg2FourCCControl = MovMpeg2FourCCControl' Lude.Text
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

pattern Xdcam :: MovMpeg2FourCCControl
pattern Xdcam = MovMpeg2FourCCControl' "XDCAM"

pattern Mpeg :: MovMpeg2FourCCControl
pattern Mpeg = MovMpeg2FourCCControl' "MPEG"

{-# COMPLETE
  Xdcam,
  Mpeg,
  MovMpeg2FourCCControl'
  #-}

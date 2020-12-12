{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H265GopSizeUnits
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.H265GopSizeUnits
  ( H265GopSizeUnits
      ( H265GopSizeUnits',
        HGSUFrames,
        HGSUSeconds
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | H265 Gop Size Units
newtype H265GopSizeUnits = H265GopSizeUnits' Lude.Text
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

pattern HGSUFrames :: H265GopSizeUnits
pattern HGSUFrames = H265GopSizeUnits' "FRAMES"

pattern HGSUSeconds :: H265GopSizeUnits
pattern HGSUSeconds = H265GopSizeUnits' "SECONDS"

{-# COMPLETE
  HGSUFrames,
  HGSUSeconds,
  H265GopSizeUnits'
  #-}

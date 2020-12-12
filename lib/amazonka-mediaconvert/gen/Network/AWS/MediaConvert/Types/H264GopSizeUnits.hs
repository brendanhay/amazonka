{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H264GopSizeUnits
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.H264GopSizeUnits
  ( H264GopSizeUnits
      ( H264GopSizeUnits',
        HGSUFrames,
        HGSUSeconds
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Indicates if the GOP Size in H264 is specified in frames or seconds. If seconds the system will convert the GOP Size into a frame count at run time.
newtype H264GopSizeUnits = H264GopSizeUnits' Lude.Text
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

pattern HGSUFrames :: H264GopSizeUnits
pattern HGSUFrames = H264GopSizeUnits' "FRAMES"

pattern HGSUSeconds :: H264GopSizeUnits
pattern HGSUSeconds = H264GopSizeUnits' "SECONDS"

{-# COMPLETE
  HGSUFrames,
  HGSUSeconds,
  H264GopSizeUnits'
  #-}

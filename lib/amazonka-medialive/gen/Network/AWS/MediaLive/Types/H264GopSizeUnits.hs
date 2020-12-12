{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H264GopSizeUnits
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.H264GopSizeUnits
  ( H264GopSizeUnits
      ( H264GopSizeUnits',
        HFrames,
        HSeconds
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | H264 Gop Size Units
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

pattern HFrames :: H264GopSizeUnits
pattern HFrames = H264GopSizeUnits' "FRAMES"

pattern HSeconds :: H264GopSizeUnits
pattern HSeconds = H264GopSizeUnits' "SECONDS"

{-# COMPLETE
  HFrames,
  HSeconds,
  H264GopSizeUnits'
  #-}

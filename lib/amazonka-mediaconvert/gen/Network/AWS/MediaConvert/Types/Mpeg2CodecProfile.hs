-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Mpeg2CodecProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Mpeg2CodecProfile
  ( Mpeg2CodecProfile
      ( Mpeg2CodecProfile',
        Main,
        Profile422
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Use Profile (Mpeg2CodecProfile) to set the MPEG-2 profile for the video output.
newtype Mpeg2CodecProfile = Mpeg2CodecProfile' Lude.Text
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

pattern Main :: Mpeg2CodecProfile
pattern Main = Mpeg2CodecProfile' "MAIN"

pattern Profile422 :: Mpeg2CodecProfile
pattern Profile422 = Mpeg2CodecProfile' "PROFILE_422"

{-# COMPLETE
  Main,
  Profile422,
  Mpeg2CodecProfile'
  #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.M2tsAudioInterval
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.M2tsAudioInterval
  ( M2tsAudioInterval
      ( M2tsAudioInterval',
        VideoAndFixedIntervals,
        VideoInterval
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | M2ts Audio Interval
newtype M2tsAudioInterval = M2tsAudioInterval' Lude.Text
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

pattern VideoAndFixedIntervals :: M2tsAudioInterval
pattern VideoAndFixedIntervals = M2tsAudioInterval' "VIDEO_AND_FIXED_INTERVALS"

pattern VideoInterval :: M2tsAudioInterval
pattern VideoInterval = M2tsAudioInterval' "VIDEO_INTERVAL"

{-# COMPLETE
  VideoAndFixedIntervals,
  VideoInterval,
  M2tsAudioInterval'
  #-}

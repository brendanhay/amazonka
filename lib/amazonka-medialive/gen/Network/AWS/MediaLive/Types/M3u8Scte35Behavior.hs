-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.M3u8Scte35Behavior
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.M3u8Scte35Behavior
  ( M3u8Scte35Behavior
      ( M3u8Scte35Behavior',
        MSBNoPassthrough,
        MSBPassthrough
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | M3u8 Scte35 Behavior
newtype M3u8Scte35Behavior = M3u8Scte35Behavior' Lude.Text
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

pattern MSBNoPassthrough :: M3u8Scte35Behavior
pattern MSBNoPassthrough = M3u8Scte35Behavior' "NO_PASSTHROUGH"

pattern MSBPassthrough :: M3u8Scte35Behavior
pattern MSBPassthrough = M3u8Scte35Behavior' "PASSTHROUGH"

{-# COMPLETE
  MSBNoPassthrough,
  MSBPassthrough,
  M3u8Scte35Behavior'
  #-}

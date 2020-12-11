-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H264TemporalAq
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.H264TemporalAq
  ( H264TemporalAq
      ( H264TemporalAq',
        HTADisabled,
        HTAEnabled
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | H264 Temporal Aq
newtype H264TemporalAq = H264TemporalAq' Lude.Text
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

pattern HTADisabled :: H264TemporalAq
pattern HTADisabled = H264TemporalAq' "DISABLED"

pattern HTAEnabled :: H264TemporalAq
pattern HTAEnabled = H264TemporalAq' "ENABLED"

{-# COMPLETE
  HTADisabled,
  HTAEnabled,
  H264TemporalAq'
  #-}

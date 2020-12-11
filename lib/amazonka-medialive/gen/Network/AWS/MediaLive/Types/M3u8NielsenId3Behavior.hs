-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.M3u8NielsenId3Behavior
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.M3u8NielsenId3Behavior
  ( M3u8NielsenId3Behavior
      ( M3u8NielsenId3Behavior',
        MNoPassthrough,
        MPassthrough
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | M3u8 Nielsen Id3 Behavior
newtype M3u8NielsenId3Behavior = M3u8NielsenId3Behavior' Lude.Text
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

pattern MNoPassthrough :: M3u8NielsenId3Behavior
pattern MNoPassthrough = M3u8NielsenId3Behavior' "NO_PASSTHROUGH"

pattern MPassthrough :: M3u8NielsenId3Behavior
pattern MPassthrough = M3u8NielsenId3Behavior' "PASSTHROUGH"

{-# COMPLETE
  MNoPassthrough,
  MPassthrough,
  M3u8NielsenId3Behavior'
  #-}

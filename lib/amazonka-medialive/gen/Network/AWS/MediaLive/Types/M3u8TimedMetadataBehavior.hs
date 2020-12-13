{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.M3u8TimedMetadataBehavior
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.M3u8TimedMetadataBehavior
  ( M3u8TimedMetadataBehavior
      ( M3u8TimedMetadataBehavior',
        MTMBfNoPassthrough,
        MTMBfPassthrough
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | M3u8 Timed Metadata Behavior
newtype M3u8TimedMetadataBehavior = M3u8TimedMetadataBehavior' Lude.Text
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

pattern MTMBfNoPassthrough :: M3u8TimedMetadataBehavior
pattern MTMBfNoPassthrough = M3u8TimedMetadataBehavior' "NO_PASSTHROUGH"

pattern MTMBfPassthrough :: M3u8TimedMetadataBehavior
pattern MTMBfPassthrough = M3u8TimedMetadataBehavior' "PASSTHROUGH"

{-# COMPLETE
  MTMBfNoPassthrough,
  MTMBfPassthrough,
  M3u8TimedMetadataBehavior'
  #-}

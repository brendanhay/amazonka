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
        M3u8TimedMetadataBehaviorNoPassthrough,
        M3u8TimedMetadataBehaviorPassthrough,
        fromM3u8TimedMetadataBehavior
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | M3u8 Timed Metadata Behavior
newtype M3u8TimedMetadataBehavior = M3u8TimedMetadataBehavior'
  { fromM3u8TimedMetadataBehavior ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern M3u8TimedMetadataBehaviorNoPassthrough :: M3u8TimedMetadataBehavior
pattern M3u8TimedMetadataBehaviorNoPassthrough = M3u8TimedMetadataBehavior' "NO_PASSTHROUGH"

pattern M3u8TimedMetadataBehaviorPassthrough :: M3u8TimedMetadataBehavior
pattern M3u8TimedMetadataBehaviorPassthrough = M3u8TimedMetadataBehavior' "PASSTHROUGH"

{-# COMPLETE
  M3u8TimedMetadataBehaviorNoPassthrough,
  M3u8TimedMetadataBehaviorPassthrough,
  M3u8TimedMetadataBehavior'
  #-}

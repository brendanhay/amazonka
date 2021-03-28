{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.M3u8Scte35Behavior
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.M3u8Scte35Behavior
  ( M3u8Scte35Behavior
    ( M3u8Scte35Behavior'
    , M3u8Scte35BehaviorNoPassthrough
    , M3u8Scte35BehaviorPassthrough
    , fromM3u8Scte35Behavior
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | M3u8 Scte35 Behavior
newtype M3u8Scte35Behavior = M3u8Scte35Behavior'{fromM3u8Scte35Behavior
                                                 :: Core.Text}
                               deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                               Core.Generic)
                               deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                 Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                 Core.FromJSON, Core.ToXML, Core.FromXML,
                                                 Core.ToText, Core.FromText, Core.ToByteString,
                                                 Core.ToQuery, Core.ToHeader)

pattern M3u8Scte35BehaviorNoPassthrough :: M3u8Scte35Behavior
pattern M3u8Scte35BehaviorNoPassthrough = M3u8Scte35Behavior' "NO_PASSTHROUGH"

pattern M3u8Scte35BehaviorPassthrough :: M3u8Scte35Behavior
pattern M3u8Scte35BehaviorPassthrough = M3u8Scte35Behavior' "PASSTHROUGH"

{-# COMPLETE 
  M3u8Scte35BehaviorNoPassthrough,

  M3u8Scte35BehaviorPassthrough,
  M3u8Scte35Behavior'
  #-}

{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.NielsenPcmToId3TaggingState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.NielsenPcmToId3TaggingState
  ( NielsenPcmToId3TaggingState
    ( NielsenPcmToId3TaggingState'
    , NielsenPcmToId3TaggingStateDisabled
    , NielsenPcmToId3TaggingStateEnabled
    , fromNielsenPcmToId3TaggingState
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | State of Nielsen PCM to ID3 tagging
newtype NielsenPcmToId3TaggingState = NielsenPcmToId3TaggingState'{fromNielsenPcmToId3TaggingState
                                                                   :: Core.Text}
                                        deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                        Core.Generic)
                                        deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                          Core.ToJSONKey, Core.FromJSONKey,
                                                          Core.ToJSON, Core.FromJSON, Core.ToXML,
                                                          Core.FromXML, Core.ToText, Core.FromText,
                                                          Core.ToByteString, Core.ToQuery,
                                                          Core.ToHeader)

pattern NielsenPcmToId3TaggingStateDisabled :: NielsenPcmToId3TaggingState
pattern NielsenPcmToId3TaggingStateDisabled = NielsenPcmToId3TaggingState' "DISABLED"

pattern NielsenPcmToId3TaggingStateEnabled :: NielsenPcmToId3TaggingState
pattern NielsenPcmToId3TaggingStateEnabled = NielsenPcmToId3TaggingState' "ENABLED"

{-# COMPLETE 
  NielsenPcmToId3TaggingStateDisabled,

  NielsenPcmToId3TaggingStateEnabled,
  NielsenPcmToId3TaggingState'
  #-}

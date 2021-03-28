{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.M2tsAudioStreamType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.M2tsAudioStreamType
  ( M2tsAudioStreamType
    ( M2tsAudioStreamType'
    , M2tsAudioStreamTypeAtsc
    , M2tsAudioStreamTypeDvb
    , fromM2tsAudioStreamType
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | M2ts Audio Stream Type
newtype M2tsAudioStreamType = M2tsAudioStreamType'{fromM2tsAudioStreamType
                                                   :: Core.Text}
                                deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                Core.Generic)
                                deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                  Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                  Core.FromJSON, Core.ToXML, Core.FromXML,
                                                  Core.ToText, Core.FromText, Core.ToByteString,
                                                  Core.ToQuery, Core.ToHeader)

pattern M2tsAudioStreamTypeAtsc :: M2tsAudioStreamType
pattern M2tsAudioStreamTypeAtsc = M2tsAudioStreamType' "ATSC"

pattern M2tsAudioStreamTypeDvb :: M2tsAudioStreamType
pattern M2tsAudioStreamTypeDvb = M2tsAudioStreamType' "DVB"

{-# COMPLETE 
  M2tsAudioStreamTypeAtsc,

  M2tsAudioStreamTypeDvb,
  M2tsAudioStreamType'
  #-}

{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H264FlickerAq
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.H264FlickerAq
  ( H264FlickerAq
    ( H264FlickerAq'
    , H264FlickerAqDisabled
    , H264FlickerAqEnabled
    , fromH264FlickerAq
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | H264 Flicker Aq
newtype H264FlickerAq = H264FlickerAq'{fromH264FlickerAq ::
                                       Core.Text}
                          deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                          Core.Generic)
                          deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                            Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                            Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                            Core.FromText, Core.ToByteString, Core.ToQuery,
                                            Core.ToHeader)

pattern H264FlickerAqDisabled :: H264FlickerAq
pattern H264FlickerAqDisabled = H264FlickerAq' "DISABLED"

pattern H264FlickerAqEnabled :: H264FlickerAq
pattern H264FlickerAqEnabled = H264FlickerAq' "ENABLED"

{-# COMPLETE 
  H264FlickerAqDisabled,

  H264FlickerAqEnabled,
  H264FlickerAq'
  #-}

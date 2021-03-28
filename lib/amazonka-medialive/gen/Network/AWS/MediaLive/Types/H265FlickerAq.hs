{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H265FlickerAq
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.H265FlickerAq
  ( H265FlickerAq
    ( H265FlickerAq'
    , H265FlickerAqDisabled
    , H265FlickerAqEnabled
    , fromH265FlickerAq
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | H265 Flicker Aq
newtype H265FlickerAq = H265FlickerAq'{fromH265FlickerAq ::
                                       Core.Text}
                          deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                          Core.Generic)
                          deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                            Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                            Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                            Core.FromText, Core.ToByteString, Core.ToQuery,
                                            Core.ToHeader)

pattern H265FlickerAqDisabled :: H265FlickerAq
pattern H265FlickerAqDisabled = H265FlickerAq' "DISABLED"

pattern H265FlickerAqEnabled :: H265FlickerAq
pattern H265FlickerAqEnabled = H265FlickerAq' "ENABLED"

{-# COMPLETE 
  H265FlickerAqDisabled,

  H265FlickerAqEnabled,
  H265FlickerAq'
  #-}

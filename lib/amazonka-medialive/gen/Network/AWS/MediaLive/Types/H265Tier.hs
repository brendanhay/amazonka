{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H265Tier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.H265Tier
  ( H265Tier
    ( H265Tier'
    , H265TierHigh
    , H265TierMain
    , fromH265Tier
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | H265 Tier
newtype H265Tier = H265Tier'{fromH265Tier :: Core.Text}
                     deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                     Core.Generic)
                     deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                       Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON, Core.FromJSON,
                                       Core.ToXML, Core.FromXML, Core.ToText, Core.FromText,
                                       Core.ToByteString, Core.ToQuery, Core.ToHeader)

pattern H265TierHigh :: H265Tier
pattern H265TierHigh = H265Tier' "HIGH"

pattern H265TierMain :: H265Tier
pattern H265TierMain = H265Tier' "MAIN"

{-# COMPLETE 
  H265TierHigh,

  H265TierMain,
  H265Tier'
  #-}

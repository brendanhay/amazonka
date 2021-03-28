{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.CmafMpdProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.CmafMpdProfile
  ( CmafMpdProfile
    ( CmafMpdProfile'
    , CmafMpdProfileMainProfile
    , CmafMpdProfileOnDemandProfile
    , fromCmafMpdProfile
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Specify whether your DASH profile is on-demand or main. When you choose Main profile (MAIN_PROFILE), the service signals  urn:mpeg:dash:profile:isoff-main:2011 in your .mpd DASH manifest. When you choose On-demand (ON_DEMAND_PROFILE), the service signals urn:mpeg:dash:profile:isoff-on-demand:2011 in your .mpd. When you choose On-demand, you must also set the output group setting Segment control (SegmentControl) to Single file (SINGLE_FILE).
newtype CmafMpdProfile = CmafMpdProfile'{fromCmafMpdProfile ::
                                         Core.Text}
                           deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                           Core.Generic)
                           deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                             Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                             Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                             Core.FromText, Core.ToByteString, Core.ToQuery,
                                             Core.ToHeader)

pattern CmafMpdProfileMainProfile :: CmafMpdProfile
pattern CmafMpdProfileMainProfile = CmafMpdProfile' "MAIN_PROFILE"

pattern CmafMpdProfileOnDemandProfile :: CmafMpdProfile
pattern CmafMpdProfileOnDemandProfile = CmafMpdProfile' "ON_DEMAND_PROFILE"

{-# COMPLETE 
  CmafMpdProfileMainProfile,

  CmafMpdProfileOnDemandProfile,
  CmafMpdProfile'
  #-}

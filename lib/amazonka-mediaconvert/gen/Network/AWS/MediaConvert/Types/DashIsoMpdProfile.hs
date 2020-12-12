{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.DashIsoMpdProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.DashIsoMpdProfile
  ( DashIsoMpdProfile
      ( DashIsoMpdProfile',
        DIMPMainProfile,
        DIMPOnDemandProfile
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Specify whether your DASH profile is on-demand or main. When you choose Main profile (MAIN_PROFILE), the service signals  urn:mpeg:dash:profile:isoff-main:2011 in your .mpd DASH manifest. When you choose On-demand (ON_DEMAND_PROFILE), the service signals urn:mpeg:dash:profile:isoff-on-demand:2011 in your .mpd. When you choose On-demand, you must also set the output group setting Segment control (SegmentControl) to Single file (SINGLE_FILE).
newtype DashIsoMpdProfile = DashIsoMpdProfile' Lude.Text
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

pattern DIMPMainProfile :: DashIsoMpdProfile
pattern DIMPMainProfile = DashIsoMpdProfile' "MAIN_PROFILE"

pattern DIMPOnDemandProfile :: DashIsoMpdProfile
pattern DIMPOnDemandProfile = DashIsoMpdProfile' "ON_DEMAND_PROFILE"

{-# COMPLETE
  DIMPMainProfile,
  DIMPOnDemandProfile,
  DashIsoMpdProfile'
  #-}

{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.DolbyVisionProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.DolbyVisionProfile
  ( DolbyVisionProfile
    ( DolbyVisionProfile'
    , DolbyVisionProfileProfile5
    , fromDolbyVisionProfile
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | In the current MediaConvert implementation, the Dolby Vision profile is always 5 (PROFILE_5). Therefore, all of your inputs must contain Dolby Vision frame interleaved data.
newtype DolbyVisionProfile = DolbyVisionProfile'{fromDolbyVisionProfile
                                                 :: Core.Text}
                               deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                               Core.Generic)
                               deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                 Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                 Core.FromJSON, Core.ToXML, Core.FromXML,
                                                 Core.ToText, Core.FromText, Core.ToByteString,
                                                 Core.ToQuery, Core.ToHeader)

pattern DolbyVisionProfileProfile5 :: DolbyVisionProfile
pattern DolbyVisionProfileProfile5 = DolbyVisionProfile' "PROFILE_5"

{-# COMPLETE 
  DolbyVisionProfileProfile5,
  DolbyVisionProfile'
  #-}

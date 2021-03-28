{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputResolution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.InputResolution
  ( InputResolution
    ( InputResolution'
    , InputResolutionSD
    , InputResolutionHD
    , InputResolutionUhd
    , fromInputResolution
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Input resolution based on lines of vertical resolution in the input; SD is less than 720 lines, HD is 720 to 1080 lines, UHD is greater than 1080 lines
newtype InputResolution = InputResolution'{fromInputResolution ::
                                           Core.Text}
                            deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                            Core.Generic)
                            deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                              Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                              Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                              Core.FromText, Core.ToByteString, Core.ToQuery,
                                              Core.ToHeader)

pattern InputResolutionSD :: InputResolution
pattern InputResolutionSD = InputResolution' "SD"

pattern InputResolutionHD :: InputResolution
pattern InputResolutionHD = InputResolution' "HD"

pattern InputResolutionUhd :: InputResolution
pattern InputResolutionUhd = InputResolution' "UHD"

{-# COMPLETE 
  InputResolutionSD,

  InputResolutionHD,

  InputResolutionUhd,
  InputResolution'
  #-}

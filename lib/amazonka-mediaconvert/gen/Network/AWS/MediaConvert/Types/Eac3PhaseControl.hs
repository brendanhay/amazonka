{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Eac3PhaseControl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.Eac3PhaseControl
  ( Eac3PhaseControl
    ( Eac3PhaseControl'
    , Eac3PhaseControlShift90Degrees
    , Eac3PhaseControlNoShift
    , fromEac3PhaseControl
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Controls the amount of phase-shift applied to the surround channels. Only used for 3/2 coding mode.
newtype Eac3PhaseControl = Eac3PhaseControl'{fromEac3PhaseControl
                                             :: Core.Text}
                             deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                             Core.Generic)
                             deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                               Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                               Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                               Core.FromText, Core.ToByteString, Core.ToQuery,
                                               Core.ToHeader)

pattern Eac3PhaseControlShift90Degrees :: Eac3PhaseControl
pattern Eac3PhaseControlShift90Degrees = Eac3PhaseControl' "SHIFT_90_DEGREES"

pattern Eac3PhaseControlNoShift :: Eac3PhaseControl
pattern Eac3PhaseControlNoShift = Eac3PhaseControl' "NO_SHIFT"

{-# COMPLETE 
  Eac3PhaseControlShift90Degrees,

  Eac3PhaseControlNoShift,
  Eac3PhaseControl'
  #-}

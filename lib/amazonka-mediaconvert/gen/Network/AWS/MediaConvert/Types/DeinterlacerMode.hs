{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.DeinterlacerMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.DeinterlacerMode
  ( DeinterlacerMode
    ( DeinterlacerMode'
    , DeinterlacerModeDeinterlace
    , DeinterlacerModeInverseTelecine
    , DeinterlacerModeAdaptive
    , fromDeinterlacerMode
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Use Deinterlacer (DeinterlaceMode) to choose how the service will do deinterlacing. Default is Deinterlace. - Deinterlace converts interlaced to progressive. - Inverse telecine converts Hard Telecine 29.97i to progressive 23.976p. - Adaptive auto-detects and converts to progressive.
newtype DeinterlacerMode = DeinterlacerMode'{fromDeinterlacerMode
                                             :: Core.Text}
                             deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                             Core.Generic)
                             deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                               Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                               Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                               Core.FromText, Core.ToByteString, Core.ToQuery,
                                               Core.ToHeader)

pattern DeinterlacerModeDeinterlace :: DeinterlacerMode
pattern DeinterlacerModeDeinterlace = DeinterlacerMode' "DEINTERLACE"

pattern DeinterlacerModeInverseTelecine :: DeinterlacerMode
pattern DeinterlacerModeInverseTelecine = DeinterlacerMode' "INVERSE_TELECINE"

pattern DeinterlacerModeAdaptive :: DeinterlacerMode
pattern DeinterlacerModeAdaptive = DeinterlacerMode' "ADAPTIVE"

{-# COMPLETE 
  DeinterlacerModeDeinterlace,

  DeinterlacerModeInverseTelecine,

  DeinterlacerModeAdaptive,
  DeinterlacerMode'
  #-}

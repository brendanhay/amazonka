{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.DeinterlacerMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.DeinterlacerMode
  ( DeinterlacerMode
      ( DeinterlacerMode',
        DMAdaptive,
        DMDeinterlace,
        DMInverseTelecine
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Use Deinterlacer (DeinterlaceMode) to choose how the service will do deinterlacing. Default is Deinterlace. - Deinterlace converts interlaced to progressive. - Inverse telecine converts Hard Telecine 29.97i to progressive 23.976p. - Adaptive auto-detects and converts to progressive.
newtype DeinterlacerMode = DeinterlacerMode' Lude.Text
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

pattern DMAdaptive :: DeinterlacerMode
pattern DMAdaptive = DeinterlacerMode' "ADAPTIVE"

pattern DMDeinterlace :: DeinterlacerMode
pattern DMDeinterlace = DeinterlacerMode' "DEINTERLACE"

pattern DMInverseTelecine :: DeinterlacerMode
pattern DMInverseTelecine = DeinterlacerMode' "INVERSE_TELECINE"

{-# COMPLETE
  DMAdaptive,
  DMDeinterlace,
  DMInverseTelecine,
  DeinterlacerMode'
  #-}

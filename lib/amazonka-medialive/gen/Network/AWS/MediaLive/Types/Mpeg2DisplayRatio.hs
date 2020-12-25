{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Mpeg2DisplayRatio
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Mpeg2DisplayRatio
  ( Mpeg2DisplayRatio
      ( Mpeg2DisplayRatio',
        Mpeg2DisplayRatioDISPLAYRATIO16X9,
        Mpeg2DisplayRatioDISPLAYRATIO4X3,
        fromMpeg2DisplayRatio
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Mpeg2 Display Ratio
newtype Mpeg2DisplayRatio = Mpeg2DisplayRatio'
  { fromMpeg2DisplayRatio ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern Mpeg2DisplayRatioDISPLAYRATIO16X9 :: Mpeg2DisplayRatio
pattern Mpeg2DisplayRatioDISPLAYRATIO16X9 = Mpeg2DisplayRatio' "DISPLAYRATIO16X9"

pattern Mpeg2DisplayRatioDISPLAYRATIO4X3 :: Mpeg2DisplayRatio
pattern Mpeg2DisplayRatioDISPLAYRATIO4X3 = Mpeg2DisplayRatio' "DISPLAYRATIO4X3"

{-# COMPLETE
  Mpeg2DisplayRatioDISPLAYRATIO16X9,
  Mpeg2DisplayRatioDISPLAYRATIO4X3,
  Mpeg2DisplayRatio'
  #-}

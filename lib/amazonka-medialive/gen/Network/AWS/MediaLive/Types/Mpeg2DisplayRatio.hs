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
        DISPLAYRATIO16X9,
        DISPLAYRATIO4X3
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Mpeg2 Display Ratio
newtype Mpeg2DisplayRatio = Mpeg2DisplayRatio' Lude.Text
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

pattern DISPLAYRATIO16X9 :: Mpeg2DisplayRatio
pattern DISPLAYRATIO16X9 = Mpeg2DisplayRatio' "DISPLAYRATIO16X9"

pattern DISPLAYRATIO4X3 :: Mpeg2DisplayRatio
pattern DISPLAYRATIO4X3 = Mpeg2DisplayRatio' "DISPLAYRATIO4X3"

{-# COMPLETE
  DISPLAYRATIO16X9,
  DISPLAYRATIO4X3,
  Mpeg2DisplayRatio'
  #-}

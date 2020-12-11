-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Vc3InterlaceMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Vc3InterlaceMode
  ( Vc3InterlaceMode
      ( Vc3InterlaceMode',
        VIMInterlaced,
        VIMProgressive
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Optional. Choose the scan line type for this output. If you don't specify a value, MediaConvert will create a progressive output.
newtype Vc3InterlaceMode = Vc3InterlaceMode' Lude.Text
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

pattern VIMInterlaced :: Vc3InterlaceMode
pattern VIMInterlaced = Vc3InterlaceMode' "INTERLACED"

pattern VIMProgressive :: Vc3InterlaceMode
pattern VIMProgressive = Vc3InterlaceMode' "PROGRESSIVE"

{-# COMPLETE
  VIMInterlaced,
  VIMProgressive,
  Vc3InterlaceMode'
  #-}

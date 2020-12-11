-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.OrientationCorrection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.OrientationCorrection
  ( OrientationCorrection
      ( OrientationCorrection',
        Rotate0,
        Rotate180,
        Rotate270,
        Rotate90
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype OrientationCorrection = OrientationCorrection' Lude.Text
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

pattern Rotate0 :: OrientationCorrection
pattern Rotate0 = OrientationCorrection' "ROTATE_0"

pattern Rotate180 :: OrientationCorrection
pattern Rotate180 = OrientationCorrection' "ROTATE_180"

pattern Rotate270 :: OrientationCorrection
pattern Rotate270 = OrientationCorrection' "ROTATE_270"

pattern Rotate90 :: OrientationCorrection
pattern Rotate90 = OrientationCorrection' "ROTATE_90"

{-# COMPLETE
  Rotate0,
  Rotate180,
  Rotate270,
  Rotate90,
  OrientationCorrection'
  #-}

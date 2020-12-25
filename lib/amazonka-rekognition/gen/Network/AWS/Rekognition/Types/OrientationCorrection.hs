{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
        OrientationCorrectionRotate0,
        OrientationCorrectionRotate90,
        OrientationCorrectionRotate180,
        OrientationCorrectionRotate270,
        fromOrientationCorrection
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype OrientationCorrection = OrientationCorrection'
  { fromOrientationCorrection ::
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

pattern OrientationCorrectionRotate0 :: OrientationCorrection
pattern OrientationCorrectionRotate0 = OrientationCorrection' "ROTATE_0"

pattern OrientationCorrectionRotate90 :: OrientationCorrection
pattern OrientationCorrectionRotate90 = OrientationCorrection' "ROTATE_90"

pattern OrientationCorrectionRotate180 :: OrientationCorrection
pattern OrientationCorrectionRotate180 = OrientationCorrection' "ROTATE_180"

pattern OrientationCorrectionRotate270 :: OrientationCorrection
pattern OrientationCorrectionRotate270 = OrientationCorrection' "ROTATE_270"

{-# COMPLETE
  OrientationCorrectionRotate0,
  OrientationCorrectionRotate90,
  OrientationCorrectionRotate180,
  OrientationCorrectionRotate270,
  OrientationCorrection'
  #-}

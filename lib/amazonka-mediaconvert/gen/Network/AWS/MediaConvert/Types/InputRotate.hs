{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.InputRotate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.InputRotate
  ( InputRotate
      ( InputRotate',
        InputRotateDegree0,
        InputRotateDegrees90,
        InputRotateDegrees180,
        InputRotateDegrees270,
        InputRotateAuto,
        fromInputRotate
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Use Rotate (InputRotate) to specify how the service rotates your video. You can choose automatic rotation or specify a rotation. You can specify a clockwise rotation of 0, 90, 180, or 270 degrees. If your input video container is .mov or .mp4 and your input has rotation metadata, you can choose Automatic to have the service rotate your video according to the rotation specified in the metadata. The rotation must be within one degree of 90, 180, or 270 degrees. If the rotation metadata specifies any other rotation, the service will default to no rotation. By default, the service does no rotation, even if your input video has rotation metadata. The service doesn't pass through rotation metadata.
newtype InputRotate = InputRotate' {fromInputRotate :: Core.Text}
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

pattern InputRotateDegree0 :: InputRotate
pattern InputRotateDegree0 = InputRotate' "DEGREE_0"

pattern InputRotateDegrees90 :: InputRotate
pattern InputRotateDegrees90 = InputRotate' "DEGREES_90"

pattern InputRotateDegrees180 :: InputRotate
pattern InputRotateDegrees180 = InputRotate' "DEGREES_180"

pattern InputRotateDegrees270 :: InputRotate
pattern InputRotateDegrees270 = InputRotate' "DEGREES_270"

pattern InputRotateAuto :: InputRotate
pattern InputRotateAuto = InputRotate' "AUTO"

{-# COMPLETE
  InputRotateDegree0,
  InputRotateDegrees90,
  InputRotateDegrees180,
  InputRotateDegrees270,
  InputRotateAuto,
  InputRotate'
  #-}

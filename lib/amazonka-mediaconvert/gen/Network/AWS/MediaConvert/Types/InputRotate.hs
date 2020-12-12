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
        Auto,
        Degree0,
        Degrees180,
        Degrees270,
        Degrees90
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Use Rotate (InputRotate) to specify how the service rotates your video. You can choose automatic rotation or specify a rotation. You can specify a clockwise rotation of 0, 90, 180, or 270 degrees. If your input video container is .mov or .mp4 and your input has rotation metadata, you can choose Automatic to have the service rotate your video according to the rotation specified in the metadata. The rotation must be within one degree of 90, 180, or 270 degrees. If the rotation metadata specifies any other rotation, the service will default to no rotation. By default, the service does no rotation, even if your input video has rotation metadata. The service doesn't pass through rotation metadata.
newtype InputRotate = InputRotate' Lude.Text
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

pattern Auto :: InputRotate
pattern Auto = InputRotate' "AUTO"

pattern Degree0 :: InputRotate
pattern Degree0 = InputRotate' "DEGREE_0"

pattern Degrees180 :: InputRotate
pattern Degrees180 = InputRotate' "DEGREES_180"

pattern Degrees270 :: InputRotate
pattern Degrees270 = InputRotate' "DEGREES_270"

pattern Degrees90 :: InputRotate
pattern Degrees90 = InputRotate' "DEGREES_90"

{-# COMPLETE
  Auto,
  Degree0,
  Degrees180,
  Degrees270,
  Degrees90,
  InputRotate'
  #-}

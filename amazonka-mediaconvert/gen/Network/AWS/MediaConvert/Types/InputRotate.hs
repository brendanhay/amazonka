{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.InputRotate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.InputRotate
  ( InputRotate
      ( ..,
        InputRotate_AUTO,
        InputRotate_DEGREES_180,
        InputRotate_DEGREES_270,
        InputRotate_DEGREES_90,
        InputRotate_DEGREE_0
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | Use Rotate (InputRotate) to specify how the service rotates your video.
-- You can choose automatic rotation or specify a rotation. You can specify
-- a clockwise rotation of 0, 90, 180, or 270 degrees. If your input video
-- container is .mov or .mp4 and your input has rotation metadata, you can
-- choose Automatic to have the service rotate your video according to the
-- rotation specified in the metadata. The rotation must be within one
-- degree of 90, 180, or 270 degrees. If the rotation metadata specifies
-- any other rotation, the service will default to no rotation. By default,
-- the service does no rotation, even if your input video has rotation
-- metadata. The service doesn\'t pass through rotation metadata.
newtype InputRotate = InputRotate'
  { fromInputRotate ::
      Core.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern InputRotate_AUTO :: InputRotate
pattern InputRotate_AUTO = InputRotate' "AUTO"

pattern InputRotate_DEGREES_180 :: InputRotate
pattern InputRotate_DEGREES_180 = InputRotate' "DEGREES_180"

pattern InputRotate_DEGREES_270 :: InputRotate
pattern InputRotate_DEGREES_270 = InputRotate' "DEGREES_270"

pattern InputRotate_DEGREES_90 :: InputRotate
pattern InputRotate_DEGREES_90 = InputRotate' "DEGREES_90"

pattern InputRotate_DEGREE_0 :: InputRotate
pattern InputRotate_DEGREE_0 = InputRotate' "DEGREE_0"

{-# COMPLETE
  InputRotate_AUTO,
  InputRotate_DEGREES_180,
  InputRotate_DEGREES_270,
  InputRotate_DEGREES_90,
  InputRotate_DEGREE_0,
  InputRotate'
  #-}

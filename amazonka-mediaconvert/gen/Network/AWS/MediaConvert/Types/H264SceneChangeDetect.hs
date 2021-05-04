{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H264SceneChangeDetect
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.H264SceneChangeDetect
  ( H264SceneChangeDetect
      ( ..,
        H264SceneChangeDetect_DISABLED,
        H264SceneChangeDetect_ENABLED,
        H264SceneChangeDetect_TRANSITION_DETECTION
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | Enable this setting to insert I-frames at scene changes that the service
-- automatically detects. This improves video quality and is enabled by
-- default. If this output uses QVBR, choose Transition detection
-- (TRANSITION_DETECTION) for further video quality improvement. For more
-- information about QVBR, see
-- https:\/\/docs.aws.amazon.com\/console\/mediaconvert\/cbr-vbr-qvbr.
newtype H264SceneChangeDetect = H264SceneChangeDetect'
  { fromH264SceneChangeDetect ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
    )

pattern H264SceneChangeDetect_DISABLED :: H264SceneChangeDetect
pattern H264SceneChangeDetect_DISABLED = H264SceneChangeDetect' "DISABLED"

pattern H264SceneChangeDetect_ENABLED :: H264SceneChangeDetect
pattern H264SceneChangeDetect_ENABLED = H264SceneChangeDetect' "ENABLED"

pattern H264SceneChangeDetect_TRANSITION_DETECTION :: H264SceneChangeDetect
pattern H264SceneChangeDetect_TRANSITION_DETECTION = H264SceneChangeDetect' "TRANSITION_DETECTION"

{-# COMPLETE
  H264SceneChangeDetect_DISABLED,
  H264SceneChangeDetect_ENABLED,
  H264SceneChangeDetect_TRANSITION_DETECTION,
  H264SceneChangeDetect'
  #-}

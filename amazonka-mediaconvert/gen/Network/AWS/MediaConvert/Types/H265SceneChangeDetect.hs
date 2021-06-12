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
-- Module      : Network.AWS.MediaConvert.Types.H265SceneChangeDetect
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.H265SceneChangeDetect
  ( H265SceneChangeDetect
      ( ..,
        H265SceneChangeDetect_DISABLED,
        H265SceneChangeDetect_ENABLED,
        H265SceneChangeDetect_TRANSITION_DETECTION
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | Enable this setting to insert I-frames at scene changes that the service
-- automatically detects. This improves video quality and is enabled by
-- default. If this output uses QVBR, choose Transition detection
-- (TRANSITION_DETECTION) for further video quality improvement. For more
-- information about QVBR, see
-- https:\/\/docs.aws.amazon.com\/console\/mediaconvert\/cbr-vbr-qvbr.
newtype H265SceneChangeDetect = H265SceneChangeDetect'
  { fromH265SceneChangeDetect ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern H265SceneChangeDetect_DISABLED :: H265SceneChangeDetect
pattern H265SceneChangeDetect_DISABLED = H265SceneChangeDetect' "DISABLED"

pattern H265SceneChangeDetect_ENABLED :: H265SceneChangeDetect
pattern H265SceneChangeDetect_ENABLED = H265SceneChangeDetect' "ENABLED"

pattern H265SceneChangeDetect_TRANSITION_DETECTION :: H265SceneChangeDetect
pattern H265SceneChangeDetect_TRANSITION_DETECTION = H265SceneChangeDetect' "TRANSITION_DETECTION"

{-# COMPLETE
  H265SceneChangeDetect_DISABLED,
  H265SceneChangeDetect_ENABLED,
  H265SceneChangeDetect_TRANSITION_DETECTION,
  H265SceneChangeDetect'
  #-}

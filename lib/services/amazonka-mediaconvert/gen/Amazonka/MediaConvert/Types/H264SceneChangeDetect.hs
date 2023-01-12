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
-- Module      : Amazonka.MediaConvert.Types.H264SceneChangeDetect
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.H264SceneChangeDetect
  ( H264SceneChangeDetect
      ( ..,
        H264SceneChangeDetect_DISABLED,
        H264SceneChangeDetect_ENABLED,
        H264SceneChangeDetect_TRANSITION_DETECTION
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Enable this setting to insert I-frames at scene changes that the service
-- automatically detects. This improves video quality and is enabled by
-- default. If this output uses QVBR, choose Transition detection
-- (TRANSITION_DETECTION) for further video quality improvement. For more
-- information about QVBR, see
-- https:\/\/docs.aws.amazon.com\/console\/mediaconvert\/cbr-vbr-qvbr.
newtype H264SceneChangeDetect = H264SceneChangeDetect'
  { fromH264SceneChangeDetect ::
      Data.Text
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
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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

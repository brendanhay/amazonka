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
-- Module      : Network.AWS.MediaLive.Types.CdiInputResolution
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.CdiInputResolution
  ( CdiInputResolution
      ( ..,
        CdiInputResolution_FHD,
        CdiInputResolution_HD,
        CdiInputResolution_SD,
        CdiInputResolution_UHD
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | Maximum CDI input resolution; SD is 480i and 576i up to 30
-- frames-per-second (fps), HD is 720p up to 60 fps \/ 1080i up to 30 fps,
-- FHD is 1080p up to 60 fps, UHD is 2160p up to 60 fps
newtype CdiInputResolution = CdiInputResolution'
  { fromCdiInputResolution ::
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

pattern CdiInputResolution_FHD :: CdiInputResolution
pattern CdiInputResolution_FHD = CdiInputResolution' "FHD"

pattern CdiInputResolution_HD :: CdiInputResolution
pattern CdiInputResolution_HD = CdiInputResolution' "HD"

pattern CdiInputResolution_SD :: CdiInputResolution
pattern CdiInputResolution_SD = CdiInputResolution' "SD"

pattern CdiInputResolution_UHD :: CdiInputResolution
pattern CdiInputResolution_UHD = CdiInputResolution' "UHD"

{-# COMPLETE
  CdiInputResolution_FHD,
  CdiInputResolution_HD,
  CdiInputResolution_SD,
  CdiInputResolution_UHD,
  CdiInputResolution'
  #-}
